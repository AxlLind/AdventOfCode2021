let input = "01000100010010111"

let flip_inverse s =
  let len = String.length s in
  String.init len (fun i -> if s.[len - i - 1] = '1' then '0' else '1')

let round s = s ^ "0" ^ (flip_inverse s)

let rec checksum s =
  let rec checksum_round res i s =
    if i = String.length s then res |> List.rev |> String.concat ""
    else
      let c = if s.[i] = s.[i+1] then "1" else "0" in
      checksum_round (c::res) (i+2) s
  in
  let sum = checksum_round [] 0 s in
  if String.length sum mod 2 = 0 then checksum sum else sum

let rec compute_result s size =
  if String.length s > size then String.sub s 0 size
  else
    compute_result (round s) size

let main () =
  let part1 = 272      |> compute_result input |> checksum in
  let part2 = 35651584 |> compute_result input |> checksum in
  (part1, part2)

let () = Aoc.timer main
