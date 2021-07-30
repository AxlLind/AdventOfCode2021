#!/bin/bash
# A simple script to compile and run a solution.
set -e

usage_exit() { printf "$1\nUsage: $0 01\n"; exit 1; }
[ $# -eq 1       ] || usage_exit "Missing argument."
[ -f ./src/$1.hs ] || usage_exit "File not found."

ghc -O ./src/$1.hs > /dev/null
./src/$1
