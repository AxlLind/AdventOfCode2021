let input = "Disc #1 has 17 positions; at time=0, it is at position 5.\nDisc #2 has 19 positions; at time=0, it is at position 8.\nDisc #3 has 7 positions; at time=0, it is at position 1.\nDisc #4 has 13 positions; at time=0, it is at position 7.\nDisc #5 has 5 positions; at time=0, it is at position 1.\nDisc #6 has 3 positions; at time=0, it is at position 0."

let parse_input () =
  let parse_line s =
    match s |> Str.split (Str.regexp "[A-Za-z;,#=. ]+") with
    | [_;size;_;offset] -> (int_of_string size, int_of_string offset)
    | _ -> failwith "unreachable"
  in
  input
  |> String.split_on_char '\n'
  |> List.map parse_line
  |> List.mapi (fun i (size, offset) -> (size - offset - i - 1, size))

(* From https://rosettacode.org/wiki/Chinese_remainder_theorem#OCaml *)
exception Modular_inverse
let inverse_mod a = function
  | 1 -> 1
  | b -> let rec inner a b x0 x1 =
           if a <= 1 then x1
           else if  b = 0 then raise Modular_inverse
           else inner b (a mod b) (x1 - (a / b) * x0) x0 in
         let x = inner a b 0 1 in
         if x < 0 then x + b else x

let chinese_remainder_exn congruences =
  let mtot = congruences
  |> List.map snd
  |> List.fold_left ( * ) 1 in
  let fn acc (r, n) = acc + r * inverse_mod (mtot / n) n * (mtot / n) in
  let v = congruences |> List.fold_left fn 0 in
  v mod mtot

let chinese_remainder congruences =
   try Some (chinese_remainder_exn congruences)
   with modular_inverse -> None

let main () =
  let congruences = parse_input () in
  let part1 = chinese_remainder congruences           |> Option.get |> string_of_int in
  let part2 = chinese_remainder ((4,11)::congruences) |> Option.get |> string_of_int in
  (part1, part2)

let () = Aoc.timer main
