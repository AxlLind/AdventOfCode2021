#!/bin/bash
set -euo pipefail
SCRIPT_DIR=$(realpath "$(dirname "$0")")

if [[ $# != 2 ]]; then
  echo "usage: $0 YEAR DAY"
  exit 1
fi

if [[ ! "$2" =~ ^(0[1-9]|1[0-9]|2[0-5])$ ]]; then
  echo "Not a valid day: $2"
  exit 1
fi

if [[ -z "${AOC_SESSION-""}" ]]; then
  echo "\$AOC_SESSION not set"
  exit 1
fi

TMPFILE=$(mktemp)
trap 'rm -f "$TMPFILE"' EXIT

curl "https://adventofcode.com/$1/day/${2#0}/input"            \
  -s --fail --cookie "session=$AOC_SESSION"                    \
  -A "Bash script at $(git remote -v | awk 'NR==1{print $2}')" \
  | tee "$TMPFILE"

mkdir -p "$SCRIPT_DIR/$1/inputs"
mv "$TMPFILE" "$SCRIPT_DIR/$1/inputs/$2.in"
