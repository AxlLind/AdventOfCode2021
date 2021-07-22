let input = ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

let rec next_row res i s =
  if i = String.length s then
    res |> List.rev |> String.concat ""
  else
    let a = if i = 0 then '.' else s.[i-1] in
    let c = if i + 1 = String.length s then '.' else s.[i+1] in
    let tile = match (a,s.[i],c) with
    | '^','^','.' -> "^"
    | '.','^','^' -> "^"
    | '.','.','^' -> "^"
    | '^','.','.' -> "^"
    | _ -> "." in
    next_row (tile::res) (i+1) s

let num_safe s =
  s |> String.to_seq |> Seq.filter ((=) '.') |> List.of_seq |> List.length

let rec safe_tiles s c left =
  if left = 0 then c else safe_tiles (next_row [] 0 s) (num_safe s + c) (left-1)

let main () =
  let part1 = safe_tiles input 0 40 in
  let part2 = safe_tiles input 0 400000 in
  (string_of_int part1, string_of_int part2)

let () = Aoc.timer main
