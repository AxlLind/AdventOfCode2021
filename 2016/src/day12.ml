let input = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 19 c\ncpy 11 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5"

type opcode =
  | Cpy of (bool * int) * int
  | Jnz of (bool * int) * (bool * int)
  | Inc of int
  | Dec of int

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
  | ["inc";a] -> Inc(a |> parse_reg_or_value |> snd)
  | ["dec";a] -> Dec(a |> parse_reg_or_value |> snd)
  | _ -> failwith "unreachable"

let exec_inst regs inst =
  let reg_val (is_reg, i) = if is_reg then regs.(i) else i in
  let () = match inst with
  | Cpy (src, dst) -> regs.(dst) <- reg_val src
  | Inc dst -> regs.(dst) <- regs.(dst) + 1
  | Dec dst -> regs.(dst) <- regs.(dst) - 1
  | _ -> () in
  match inst with
  | Jnz (src, off) when reg_val src != 0 -> reg_val off
  | _ -> 1

let rec exec insts regs ip =
  if ip < List.length insts then
    let offset = exec_inst regs (ip |> List.nth insts) in
    exec insts regs (ip + offset)
  else regs.(0)

let main () =
  let insts = input |> Aoc.parse_lines parse_line in
  let part1 = exec insts [|0;0;0;0|] 0 in
  let part2 = exec insts [|0;0;1;0|] 0 in
  (string_of_int part1, string_of_int part2)

let () = Aoc.timer main
