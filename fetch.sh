#!/bin/bash
set -euo pipefail
SCRIPT_DIR=$(realpath "$(dirname "$0")")

if [[ $# != 1 ]]; then
  echo Please provide a day number.
  echo usage: "$0" DAY
  exit 1
fi

if [[ ! "$1" =~ ^(0[1-9]|1[0-9]|2[0-5])$ ]]; then
  echo Not a valid day: "$1"
  exit 1
fi

if [[ -z "${AOC_SESSION-""}" ]]; then
  echo \$AOC_SESSION not set
  exit 1
fi

TMPFILE=$(mktemp)
trap "rm -f '$TMPFILE'" EXIT

curl -s "https://adventofcode.com/2023/day/${1#0}/input"       \
  --fail                                                       \
  --cookie "session=$AOC_SESSION"                              \
  -A "Bash script at $(git remote -v | awk 'NR==1{print $2}')" \
  | tee "$TMPFILE"

mkdir -p "$SCRIPT_DIR/inputs"
mv "$TMPFILE" "$SCRIPT_DIR/inputs/$1.in"
