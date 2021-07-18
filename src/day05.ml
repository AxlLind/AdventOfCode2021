let input = "uqwqemis"

let test_hash_counter c =
  let h = input ^ (string_of_int c) |> Digest.string |> Digest.to_hex in
  if String.sub h 0 5 = "00000" then Some h else None

let rec next_hash c =
  match test_hash_counter c with
  | Some(h) -> (c+1, h.[5], h.[6])
  | None -> next_hash (c+1)

let chars_to_str cs = cs |> List.map (String.make 1) |> String.concat ""

let part1 () =
  let (c1, i1, _) = next_hash 0  in
  let (c2, i2, _) = next_hash c1 in
  let (c3, i3, _) = next_hash c2 in
  let (c4, i4, _) = next_hash c3 in
  let (c5, i5, _) = next_hash c4 in
  let (c6, i6, _) = next_hash c5 in
  let (c7, i7, _) = next_hash c6 in
  let (_,  i8, _) = next_hash c7 in
  chars_to_str [i1;i2;i3;i4;i5;i6;i7;i8]

let rec find_pw digits c =
  let (c_next, i, d) = next_hash c in
  let i = (Char.code i) - 48 in
  if i < 9 && digits.(i) = None then
    let () = digits.(i) <- Some(d) in
    if digits |> Array.for_all Option.is_some then digits
    else find_pw digits c_next
  else
    find_pw digits c_next

let part2 () =
  find_pw (Array.make 8 None) 0
  |> Array.map Option.get
  |> Array.to_list
  |> chars_to_str

let main () = (part1 (), part2 ())

let () = Aoc.timer main
