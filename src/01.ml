open Format

module SS = Set.Make(struct type t = int * int let compare = compare end)

let input = "R4, R3, L3, L2, L1, R1, L1, R2, R3, L5, L5, R4, L4, R2, R4, L3, R3, L3, R3, R4, R2, L1, R2, L3, L2, L1, R3, R5, L1, L4, R2, L4, R3, R1, R2, L5, R2, L189, R5, L5, R52, R3, L1, R4, R5, R1, R4, L1, L3, R2, L2, L3, R4, R3, L2, L5, R4, R5, L2, R2, L1, L3, R3, L4, R4, R5, L1, L1, R3, L5, L2, R76, R2, R2, L1, L3, R189, L3, L4, L1, L3, R5, R4, L1, R1, L1, L1, R2, L4, R2, L5, L5, L5, R2, L4, L5, R4, R4, R5, L5, R3, L1, L3, L1, L1, L3, L4, R5, L3, R5, R3, R3, L5, L5, R3, R4, L3, R3, R1, R3, R2, R2, L1, R1, L3, L3, L3, L1, R2, L1, R4, R4, L1, L1, R3, R3, R4, R1, L5, L2, R2, R3, R2, L3, R4, L5, R1, R4, R5, R4, L4, R1, L3, R1, R3, L2, L3, R1, L2, R3, L3, L1, L3, R4, L4, L5, R3, R5, R4, R1, L2, R3, R5, L5, L4, L1, L1"

type way = Right | Left
type dir = North | East | South | West

let input_dirs =
  let parse_dir s =
    let len = String.sub s 1 ((String.length s)-1) |> int_of_string in
    match s.[0] with
    | 'R' -> (len, Right)
    | 'L' -> (len, Left)
    | _   -> failwith "unreachable"
  in
  input |> Str.split (Str.regexp ", ") |> List.map parse_dir

let update (dir,x,y) (len, way) =
  let new_d = match (dir, way) with
  | (North, Right) -> East
  | (East,  Right) -> South
  | (South, Right) -> West
  | (West,  Right) -> North
  | (North, Left)  -> West
  | (East,  Left)  -> North
  | (South, Left)  -> East
  | (West,  Left)  -> South in
  match new_d with
  | North -> (new_d, x, y+len)
  | East  -> (new_d, x+len, y)
  | South -> (new_d, x, y-len)
  | West  -> (new_d, x-len, y)

let walk_set (x1,y1) (x2,y2) =
  let positions =
    if x1 == x2 then
      let a, b = (min y1 y2, max y1 y2) in
      List.init (b - a - 1) (fun y -> (x1, a + y + 1))
    else
      let a, b = (min x1 x2, max x1 x2) in
      List.init (b - a - 1) (fun x -> (a + x + 1, y1))
  in
  positions |> List.to_seq |> SS.of_seq

let rec find_duplicate positions (dir,x,y) dirs =
  let dir2,x2,y2 = update (dir,x,y) (List.hd dirs) in
  let new_positions = walk_set (x,y) (x2,y2) in
  let overlap = SS.inter positions new_positions in
  match SS.choose_opt overlap with
  | Some(pos) -> pos
  | None -> find_duplicate (SS.union positions new_positions) (dir2,x2,y2) (List.tl dirs)

let _ =
  let _,x,y = List.fold_left update (North,0,0) input_dirs in
  printf "Part 1: %d\n" ((abs x) + (abs y))

let _ =
  let x,y = find_duplicate SS.empty (North,0,0) input_dirs in
  printf "Part 2: %d\n" ((abs x) + (abs y))
