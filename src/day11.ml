(*
Did not want to deal with parsing this, so parsed it by hand. The raw input:

The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
The third floor contains nothing relevant.
The fourth floor contains nothing relevant.
*)

(* Positive is a generator, negative a chip *)
let init_floors_1 = [ [1; 2; -2; 3; 4; -4; 5; -5]; [-1; -3]; []; [] ]
let init_floors_2 = [ [1; 2; -2; 3; 4; -4; 5; -5; 6; -6; 7; -7]; [-1; -3]; []; [] ]

let safe_floor floor =
  if floor |> List.for_all (fun x -> x < 0) then true
  else
    floor |> List.for_all (fun x -> x > 0 || floor |> List.mem (-x))

let combinations list =
  let pairs i x = list
    |> List.filteri (fun j _ -> i < j)
    |> List.map (fun y -> [x;y])
  in
  let tuples = list |> List.mapi pairs |> List.flatten in
  tuples @ (list |> List.map (fun x -> [x]))

let floor_str (floor, floors) =
  let floor_to_str f = f |> List.sort compare |> List.map string_of_int |> String.concat "," in
  let s = floors |> Array.map floor_to_str |> Array.to_list |> String.concat "|" in
  (string_of_int floor) ^ "|" ^ s

let apply_move visited floors floor dir move =
  let elevator_safe = match move with
    | [x;y] when x < 0 && y > 0 -> x + y = 0
    | [x;y] when y < 0 && x > 0 -> x + y = 0
    | _ -> true in
  if not elevator_safe then None
  else
    let new_floors = Array.copy floors in
    new_floors.(floor) <- floors.(floor) |> List.filter (fun x -> List.mem x move |> not);
    new_floors.(floor + dir) <- move @ floors.(floor+dir);
    let valid = safe_floor new_floors.(floor)
      && safe_floor new_floors.(floor + dir)
      && (floor+dir,new_floors) |> floor_str |> Hashtbl.mem visited |> not in
    if valid then Some (floor+dir,new_floors) else None

let valid_moves visited floors floor =
  let moves = combinations floors.(floor) in
  let dirs = match floor with
    | 0 -> [1]
    | 3 -> [-1]
    | _ -> [-1;1] in
  dirs
  |> List.map (fun dir -> moves |> List.filter_map (apply_move visited floors floor dir))
  |> List.flatten

let rec bfs visited queue (floor, floors) steps =
  if floor = 3 && floors.(0) = [] && floors.(1) = [] && floors.(2) = [] then steps
  else
    let neighbours = valid_moves visited floors floor in
    neighbours |> List.iter (fun n -> Hashtbl.add visited (floor_str n) true);
    neighbours |> List.to_seq |> Seq.map (fun n -> (steps+1, n)) |> Queue.add_seq queue;
    let steps, next = Queue.take queue in
    bfs visited queue next steps

let main () =
  let part1 = bfs (Hashtbl.create 2024) (Queue.create ()) (0, init_floors_1 |> Array.of_list) 0 in
  let part2 = bfs (Hashtbl.create 2024) (Queue.create ()) (0, init_floors_2 |> Array.of_list) 0 in
  (part1 |> string_of_int, part2 |> string_of_int)

let () = Aoc.timer main
