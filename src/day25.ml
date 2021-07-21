let input = "cpy a d\ncpy 4 c\ncpy 633 b\ninc d\ndec b\njnz b -2\ndec c\njnz c -5\ncpy d a\njnz 0 0\ncpy a b\ncpy 0 a\ncpy 2 c\njnz b 2\njnz 1 6\ndec b\ndec c\njnz c -4\ninc a\njnz 1 -7\ncpy 2 b\njnz c 2\njnz 1 4\ndec b\ndec c\njnz 1 -4\njnz 0 0\nout b\njnz a -19\njnz 1 -21"

type opcode =
  | Cpy of (bool * int) * int
  | Jnz of (bool * int) * (bool * int)
  | Out of (bool * int)
  | Inc of int
  | Dec of int

let parse_input () =
  let parse_reg_or_value s = match s.[0] with
  | 'a' -> (true, 0)
  | 'b' -> (true, 1)
  | 'c' -> (true, 2)
  | 'd' -> (true, 3)
  | _   -> (false, int_of_string s) in
  let parse_line s = match s |> String.split_on_char ' ' with
  | ["cpy";a;b] -> Cpy(parse_reg_or_value a, b |> parse_reg_or_value |> snd)
  | ["jnz";a;b] -> Jnz(parse_reg_or_value a, parse_reg_or_value b)
  | ["out";a] -> Out(a |> parse_reg_or_value)
  | ["inc";a] -> Inc(a |> parse_reg_or_value |> snd)
  | ["dec";a] -> Dec(a |> parse_reg_or_value |> snd)
  | _ -> failwith "unreachable"
  in
  input |> String.split_on_char '\n' |> List.map parse_line

let rec exec res regs ip insts =
  let reg_val (is_reg, i) = if is_reg then regs.(i) else i in
  if ip < Array.length insts && List.length res < 50 then
    let () = match insts.(ip) with
    | Cpy (src, dst) -> regs.(dst) <- reg_val src
    | Inc dst -> regs.(dst) <- regs.(dst) + 1
    | Dec dst -> regs.(dst) <- regs.(dst) - 1
    | _ -> () in
    let offset = match insts.(ip) with
    | Jnz (src, off) -> if reg_val src = 0 then 1 else reg_val off
    | _ -> 1 in
    let res = match insts.(ip) with
    | Out src -> (reg_val src)::res
    | _ -> res in
    exec res regs (ip + offset) insts
  else res

let rec check_output = function
  | a::b::xs -> a + b = 1 && check_output (b::xs)
  | _ -> true

let rec find insts i =
  if insts |> exec [] [|i;0;0;0|] 0 |> check_output then i
  else find insts (i+1)

let main () =
  let insts = parse_input () |> Array.of_list in
  let part1 = find insts 0 |> string_of_int in
  (part1, "🎄")

let () = Aoc.timer main
