# Fetches the input, prints to stdout and copies to clipboard.
# This gives both a quick overview, and makes it available to copy.
curl "https://adventofcode.com/2020/day/$1/input" \
  --cookie "session=$AOC_SESSION" -s              \
  | tee /dev/tty                                  \
  | pbcopy
