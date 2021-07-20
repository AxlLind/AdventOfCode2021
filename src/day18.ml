let input = ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

let rec next_row res i s =
  if i = String.length s then
    res |> List.rev |> String.concat ""
  else
    let a = if i = 0 then '.' else s.[i-1] in
    let c = if i + 1 = String.length s then '.' else s.[i+1] in
    let tile = match [a;s.[i];c] with
    | ['^'; '^'; '.'] -> "^"
    | ['.'; '^'; '^'] -> "^"
    | ['.'; '.'; '^'] -> "^"
    | ['^'; '.'; '.'] -> "^"
    | _ -> "." in
    next_row (tile::res) (i+1) s

let num_safe s =
  s |> String.to_seq |> Seq.filter ((=) '.') |> List.of_seq |> List.length

let rec safe_tiles c s left =
  if left = 0 then c else safe_tiles (s |> num_safe |> (+) c) (next_row [] 0 s) (left-1)

let main () =
  let part1 = 40     |> safe_tiles 0 input |> string_of_int in
  let part2 = 400000 |> safe_tiles 0 input |> string_of_int in
  (part1,part2)

let () = Aoc.timer main
