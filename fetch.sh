#!/bin/bash

if [ -z "$1" ]; then
  echo "Please provide a day number."
  echo "usage: $0 DAY"
  exit 1
fi

if [ -z "$AOC_SESSION" ]; then
  echo "No session token."
  exit 1
fi

URL="https://adventofcode.com/2018/day/$1/input"
curl $URL --cookie "session=$AOC_SESSION" -s
