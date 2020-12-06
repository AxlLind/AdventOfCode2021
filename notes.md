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

## Day 03 - [link](./src/bin/03.rs)
You have to calculate the number of trees in a given slope. For some reason I was just quite slow on part one. I started thinking of [day 10](https://github.com/AxlLind/AdventOfCode2019/blob/master/src/bin/10.rs) of 2019, which was just a thousand times more difficult. This problem was easy, so I should have just continued.

For part one, the fact that the lines continues was easy, obviously just modulo. I chose to use a cycle iterator: `(0..W).cycle().step_by(dw)`. That might have been the only tricky part for people. After I did part one, doing part two was quite easy. In my solution I iterated over all rows since `dh == 1`. That was the only thing that was not general about my initial solution. Breaking it out into a function, general over `dw, dh` was easy otherwise.

So not my best day. I was just not that fast on this one but that's okay :)

## Day 04 - [link](./src/bin/04.rs)
This was entirely a parsing challenge. When I realized that I got super nervous since Rust is not always the smoothest when it comes to string handling in my experience. However, it was surprisingly easy to do this in Rust, I mean just look at how easy it was to parse each passport:

```Rust
let passport = s.split_whitespace()
  .flat_map(|p| p.split(':'))
  .tuples()
  .collect::<HashMap<_,_>>();
```

I was a bit slow on star one since I misread how to parse the input, but got it after a while. For star two, it was just about implementing all these rules which was also surprisingly easy in Rust. The char primitive has some amazing functions to check what type of char it is, like `c.is_ascii_digit()` and `c.is_ascii_hexdigit()`, which made it quite easy.

Overall, I placed (3468/1688), so could have been faster on star one but very happy with my performance on star two!

## Day 05 - [link](./src/bin/05.rs)
First weekend. I overslept since my alarm was only set for weekdays, so missed the leaderboard completely today. That's fine though! Not sure I'll make it through december, going up at 5:50 each day.

The input was just a disguised binary encoding. I choose to implement it like the instructions, with a binary search. For part two, I struggled a few minutes with understanding exactly what the instructions meant, but it wasn't too bad. Used a hashset for fast lookup, which made it `O(n)`. Another solution would have been to sort the list and check for a missing entry but that would be `O(nlogn)`.

## Day 06 - [link](./src/bin/06.rs)
Today was really fun! First a bit more challenging problem in my opinion. Managed to get a respectable score on both stars today which I am really happy with. Part one I realized quickly would be solved nicely with the [itertools::unique](https://docs.rs/itertools/0.9.0/itertools/trait.Itertools.html#method.unique) function:

```Rust
s.chars()
  .filter(|c| !c.is_whitespace())
  .unique()
  .count()
```

Then you just had to sum it up. This was both quick and really easy to implement, so got an ok score on the leaderboard.

Part two was a bit more difficult. Initially, I started using HashSets, and using `set1.intersection(set2)`, ran into borrow issues and quickly switched approach. The possible answers are only `a-z`, which we can easily fit in a `u32` and let each bit represent if the answer is present or not. You then just have to `and` them together, starting with the full set `0x3ffffff`. The Rust number primitives also have an amazing function [u32::count_ones()](https://doc.rust-lang.org/std/primitive.u32.html#method.count_ones) which made this approach very easy to implement.

```Rust
s.split_whitespace()
  .map(|part| part.bytes().fold(0u32, |x, b| x | 1 << (b - b'a')))
  .fold(!0, |acc, x| acc & x)
  .count_ones()
```

Not using HashSets means no extra allocations. Both stars are solved in a single iterator expression each, without any collect. My solution is therefore quite fast, `0ms` on my machine.
