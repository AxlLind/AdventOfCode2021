let input = "cpy a b\ndec b\ncpy a d\ncpy 0 a\ncpy b c\ninc a\ndec c\njnz c -2\ndec d\njnz d -5\ndec b\ncpy b c\ncpy c d\ndec d\ninc c\njnz d -2\ntgl c\ncpy -16 c\njnz 1 c\ncpy 84 c\njnz 75 d\ninc a\ninc d\njnz d -2\ninc c\njnz c -5"

type opcode =
  | Cpy of (bool * int) * int
  | Jnz of (bool * int) * (bool * int)
  | Tgl of (bool * int)
  | Inc of int
  | Dec of int
  | InvalidCpy of (bool * int) * int
  | InvalidInc

let parse_line s =
  let parse_reg_or_value s = match s.[0] with
  | 'a' -> (true, 0)
  | 'b' -> (true, 1)
  | 'c' -> (true, 2)
  | 'd' -> (true, 3)
  | _   -> (false, int_of_string s) in
  match s |> String.split_on_char ' ' with
  | ["cpy";a;b] -> Cpy(parse_reg_or_value a, b |> parse_reg_or_value |> snd)
  | ["jnz";a;b] -> Jnz(parse_reg_or_value a, parse_reg_or_value b)
  | ["tgl";a] -> Tgl(parse_reg_or_value a)
  | ["inc";a] -> Inc(a |> parse_reg_or_value |> snd)
  | ["dec";a] -> Dec(a |> parse_reg_or_value |> snd)
  | _ -> failwith "unreachable"

let toggle_inst = function
  | Cpy (src, dst) -> Jnz (src, (true, dst))
  | Jnz (src, (is_reg, off)) -> if is_reg then Cpy (src, off) else InvalidCpy (src, off)
  | InvalidCpy (src, off) -> Jnz (src, (false, off))
  | Inc dst -> Dec dst
  | Dec dst -> Inc dst
  | Tgl (is_reg, off) -> if is_reg then Inc off else InvalidInc
  | InvalidInc -> InvalidInc

let rec exec regs ip insts =
  let reg_val (is_reg, i) = if is_reg then regs.(i) else i in
  if ip < Array.length insts then
    let () = match insts.(ip) with
    | Cpy (src, dst) -> regs.(dst) <- reg_val src
    | Inc dst -> regs.(dst) <- regs.(dst) + 1
    | Dec dst -> regs.(dst) <- regs.(dst) - 1
    | _ -> () in
    let offset = match insts.(ip) with
    | Jnz (src, off) -> if reg_val src = 0 then 1 else reg_val off
    | Tgl dst ->
      let i = ip + (reg_val dst) in
      if i < Array.length insts then insts.(i) <- toggle_inst insts.(i);
      1
    | _ -> 1 in
    exec regs (ip + offset) insts
  else regs.(0)

let main () =
  let insts = input |> Aoc.parse_lines parse_line in
  let part1 = insts |> Array.of_list |> exec [|7;0;0;0|]  0 |> string_of_int in
  let part2 = insts |> Array.of_list |> exec [|12;0;0;0|] 0 |> string_of_int in
  (part1, part2)

let () = Aoc.timer main
