open Format

let parse_lines parse_line input = input |> String.split_on_char '\n' |> List.map parse_line

let timer f =
  let t0 = Sys.time() in
  let (part1, part2) = f () in
  let elapsed_ms = (Sys.time() -. t0) *. 1000.0 in
  printf "Part 1: %s\n" part1;
  printf "Part 2: %s\n" part2;
  printf "Time: %.3fms\n" elapsed_ms
