# Notes
This is my thoughts I wrote down after I completed each challenge.

## Day 01 - [link](./src/bin/01.rs)
It's december again, finally! So excited to do this again this year. Thought about maybe doing it in F# this year but decided against it. Rust again it is!

Very easy problem of course since it's day 1. Servers crashed just when I was about to submit! I think I got lucky because after a while I managed to submit my answer, and I got 54th place IN THE WORLD!!!

![leaderboard](./screenshots/leaderboard-day1.png)

Solved using a double and triple for loop. Later cleaned it up and used `tuple-combinations` from the `itertools` crate. So both a fast (`1ms`) and clean solution.

Edit: They later removed the leaderboards for day 1 since it was unfair, which makes sense. Still though!

## Day 02 - [link](./src/bin/02.rs)
This was one of those "find correct passwords" puzzles that have shown up previous years. The input was a bit more complex (`4-5 m: mmpth`). I chose to alter the input before using it: `"4-5 m: mmpth" -> (4,5,b'm',b"mmpth")`, i.e a tuple of `usize, usize, u8, &[u8]`, and hard-code it into my solution.

Otherwise, the problem itself was quite easy. Part one, count the occurrences, check if it's within the given range. Part two, look at the two chars and check if exactly 1 is equal to the provided char.

This day made it clear why Rust is not the best language for speed programming. I got silly borrowing compilation errors in filter for example, which were easy to fix but took some time. My biggest take away from today is that it is **always** worth it to read the input clearly! I wasted so much time by trying to save like 5 seconds and actually read the instructions. Got rate-limited after I submitted the wrong answer, because I did not read the question clearly. Will not do that again..
