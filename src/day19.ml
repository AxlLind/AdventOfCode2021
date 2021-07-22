let input = 3012210

let rec josephus i =
  if i < 3 then 1 else josephus (i/2) * 2 + ((i mod 2) * 2 - 1)

let rec find_pow_three i =
  if i * 3 > input then i else find_pow_three (i*3)

let main () =
  let part1 = josephus input in
  let part2 = input - (find_pow_three 1) in
  (string_of_int part1, string_of_int part2)

let () = Aoc.timer main
