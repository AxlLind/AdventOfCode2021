let input = "swap position 5 with position 6\nreverse positions 1 through 6\nrotate right 7 steps\nrotate based on position of letter c\nrotate right 7 steps\nreverse positions 0 through 4\nswap letter f with letter h\nreverse positions 1 through 2\nmove position 1 to position 0\nrotate based on position of letter f\nmove position 6 to position 3\nreverse positions 3 through 6\nrotate based on position of letter c\nrotate based on position of letter b\nmove position 2 to position 4\nswap letter b with letter d\nmove position 1 to position 6\nmove position 7 to position 1\nswap letter f with letter c\nmove position 2 to position 3\nswap position 1 with position 7\nreverse positions 3 through 5\nswap position 1 with position 4\nmove position 4 to position 7\nrotate right 4 steps\nreverse positions 3 through 6\nmove position 0 to position 6\nswap position 3 with position 5\nswap letter e with letter h\nrotate based on position of letter c\nswap position 4 with position 7\nreverse positions 0 through 5\nrotate right 5 steps\nrotate left 0 steps\nrotate based on position of letter f\nswap letter e with letter b\nrotate right 2 steps\nrotate based on position of letter c\nswap letter a with letter e\nrotate left 4 steps\nrotate left 0 steps\nmove position 6 to position 7\nrotate right 2 steps\nrotate left 6 steps\nrotate based on position of letter d\nswap letter a with letter b\nmove position 5 to position 4\nreverse positions 0 through 7\nrotate left 3 steps\nrotate based on position of letter e\nrotate based on position of letter h\nswap position 4 with position 6\nreverse positions 4 through 5\nreverse positions 5 through 7\nrotate left 3 steps\nmove position 7 to position 2\nmove position 3 to position 4\nswap letter b with letter d\nreverse positions 3 through 4\nswap letter e with letter a\nrotate left 4 steps\nswap position 3 with position 4\nswap position 7 with position 5\nrotate right 1 step\nrotate based on position of letter g\nreverse positions 0 through 3\nswap letter g with letter b\nrotate based on position of letter b\nswap letter a with letter c\nswap position 0 with position 2\nreverse positions 1 through 3\nrotate left 7 steps\nswap letter f with letter a\nmove position 5 to position 0\nreverse positions 1 through 5\nrotate based on position of letter d\nrotate based on position of letter c\nrotate left 2 steps\nswap letter b with letter a\nswap letter f with letter c\nswap letter h with letter f\nrotate based on position of letter b\nrotate left 3 steps\nswap letter b with letter h\nreverse positions 1 through 7\nrotate based on position of letter h\nswap position 1 with position 5\nrotate left 1 step\nrotate based on position of letter h\nreverse positions 0 through 1\nswap position 5 with position 7\nreverse positions 0 through 2\nreverse positions 1 through 3\nmove position 1 to position 4\nreverse positions 1 through 3\nrotate left 1 step\nswap position 4 with position 1\nmove position 1 to position 3\nrotate right 2 steps\nmove position 0 to position 5"

let str_to_chars s = s |> String.to_seq |> List.of_seq
let chars_to_str s = s |> List.map (String.make 1) |> String.concat ""

let split n list =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
                      else aux (i - 1) (h :: acc) t  in
  aux n [] list

let rotate list n =
  let len = List.length list in
  let n = ((len - n) mod len + len) mod len in
  if n = 0 then list
  else let a, b = split n list in b @ a

type inst =
  | SwapPos of int * int
  | SwapChar of char * char
  | Rotate of int
  | RotateChar of char
  | Reverse of int * int
  | Move of int * int

let parse_input () =
  let parse_line s = match s |> String.split_on_char ' ' with
  | ["swap"; "position"; x; _; _; y] -> SwapPos (int_of_string x, int_of_string y)
  | ["swap"; "letter"; x; _; _; y] -> SwapChar (x.[0], y.[0])
  | ["rotate"; way; x; _] -> Rotate (if way = "right" then int_of_string x else -(int_of_string x))
  | ["rotate"; _; _; _; _; _; x] -> RotateChar (x.[0])
  | ["reverse"; _; x; _; y] -> Reverse (int_of_string x, int_of_string y)
  | ["move"; _; x; _; _; y] -> Move (int_of_string x, int_of_string y)
  | _ -> failwith "unreachable"
  in
  input |> String.split_on_char '\n' |> List.map parse_line

let rec index e = function
  | e2::xs when e = e2 -> 0
  | _::xs -> 1 + (index e xs)
  | [] -> failwith "not found"

let rev list x y =
  let a,b = split x list in
  let (b,c) = split (y - x+1) b in
  a @ (List.rev b) @ c

let exec_inst s = function
  | SwapPos (x,y) ->
    let a = x |> List.nth s in
    let b = y |> List.nth s in
    s |> List.mapi (fun i c -> if i = x then b else if i = y then a else c)
  | SwapChar (a,b) -> s |> List.map (fun c -> if c = a then b else if c = b then a else c)
  | Rotate n -> rotate s n
  | RotateChar c ->
    let idx = index c s in
    rotate s (idx + 1 + if idx > 3 then 1 else 0)
  | Reverse (x,y) -> rev s x y
  | Move (x,y) ->
    let e = x |> List.nth s in
    let a,b = s |> List.filteri (fun i _ -> i != x) |> split y in
    a @ e::b

let rec rotate_char_inverse original s c =
  if original = (RotateChar c |> exec_inst s) then s
  else rotate_char_inverse original (rotate s (-1)) c

let exec_inst_rev s = function
  | Rotate n -> exec_inst s (Rotate (-n))
  | RotateChar c -> rotate_char_inverse s s c
  | Move (x,y) -> exec_inst s (Move (y,x))
  | inst -> exec_inst s inst

let main () =
  let insts = parse_input () in
  let part1 = insts |> List.fold_left exec_inst (str_to_chars "abcdefgh") in
  let part2 = insts |> List.rev |> List.fold_left exec_inst_rev (str_to_chars "fbgdceah") in
  (part1 |> chars_to_str, part2 |> chars_to_str)

let () = Aoc.timer main
