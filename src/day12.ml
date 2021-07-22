let input = "cpy 1 a\ncpy 1 b\ncpy 26 d\njnz c 2\njnz 1 5\ncpy 7 c\ninc d\ndec c\njnz c -2\ncpy a c\ninc a\ndec b\njnz b -2\ncpy c b\ndec d\njnz d -6\ncpy 19 c\ncpy 11 d\ninc a\ndec d\njnz d -2\ndec c\njnz c -5"

type opcode =
  | Cpy of bool * int * int
  | Inc of int
  | Dec of int
  | Jnz of int * int

let parse_line s =
  let reg_to_int s = match s.[0] with
  | 'a' -> 0
  | 'b' -> 1
  | 'c' -> 2
  | 'd' -> 3
  | _ -> failwith ("unreachable " ^ s) in
  match s |> String.split_on_char ' ' with
  | [op;a;b] ->
    let (reg,src) =
      if String.contains "abcd" a.[0] then (true, reg_to_int a)
      else (false, int_of_string a) in
    if op = "cpy" then
      Cpy(reg, src, reg_to_int b)
    else
      Jnz(src, int_of_string b)
  | [op;a] ->
    let reg = reg_to_int a in
    if op = "inc" then Inc(reg) else Dec(reg)
  | _ -> failwith "unreachable"

let exec_inst regs inst = match inst with
  | Cpy (reg_src, src, dst) -> regs.(dst) <- if reg_src then regs.(src) else src; 1
  | Inc (dst) -> regs.(dst) <- regs.(dst) + 1; 1
  | Dec (dst) -> regs.(dst) <- regs.(dst) - 1; 1
  | Jnz (src, off) -> if regs.(src) = 0 then 1 else off

let rec exec insts regs ip =
  if ip < List.length insts then
    let offset = exec_inst regs (ip |> List.nth insts) in
    exec insts regs (ip + offset)
  else regs.(0)

let main () =
  let insts = input |> Aoc.parse_lines parse_line in
  let part1 = exec insts [|0;0;0;0|] 0 |> string_of_int in
  let part2 = exec insts [|0;0;1;0|] 0 |> string_of_int in
  (part1, part2)

let () = Aoc.timer main
