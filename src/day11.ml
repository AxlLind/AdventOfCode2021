(* Did not want to deal with parsing this, so parsed it by hand.
   Positive is a generator, negative a chip *)
let init_floors_1 = [| [-5; -4; -2; 1; 2; 3; 4; 5]; [-3; -1]; []; [] |]
let init_floors_2 = [| [-7; -6; -5; -4; -2; 1; 2; 3; 4; 5; 6; 7]; [-3; -1]; []; [] |]

let safe_floor floor =
  if floor |> List.for_all (fun x -> x < 0) then true
  else floor |> List.for_all (fun x -> x > 0 || floor |> List.mem (-x))

let combinations list =
  let pairs i x = list
    |> List.filteri (fun j _ -> i < j)
    |> List.map (fun y -> [x;y])
  in
  let tuples = list |> List.mapi pairs |> List.flatten in
  tuples @ (list |> List.map (fun x -> [x]))

let state_to_string (floor, floors) =
  let id_map = Hashtbl.create 16 in
  let get_id n = match n |> abs |> Hashtbl.find_opt id_map with
    | Some id -> if n < 0 then -id else id
    | None ->
      let id = id_map |> Hashtbl.to_seq_values |> Seq.fold_left max (-1) |> (+) 1 in
      Hashtbl.add id_map (abs n) id;
      if n < 0 then -id else id
  in
  let floor_to_str f = f |> List.sort compare |> List.map get_id |> List.map string_of_int |> String.concat "" in
  let s = floors |> Array.map floor_to_str |> Array.to_list |> String.concat "|" in
  (string_of_int floor) ^ "|" ^ s

let apply_move visited floors floor dir move =
  let new_floors = Array.copy floors in
  new_floors.(floor) <- floors.(floor) |> List.filter (fun x -> List.mem x move |> not);
  new_floors.(floor + dir) <- (move @ floors.(floor+dir)) |> List.sort compare;
  let valid = safe_floor new_floors.(floor)
    && safe_floor new_floors.(floor + dir)
    && (floor+dir,new_floors) |> state_to_string |> Hashtbl.mem visited |> not in
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
  if [0;1;2] |> List.for_all (fun i -> floors.(i) = []) then steps
  else
    let neighbours = valid_moves visited floors floor in
    neighbours |> List.iter (fun n -> Hashtbl.add visited (state_to_string n) true);
    neighbours |> List.to_seq |> Seq.map (fun n -> (steps+1, n)) |> Queue.add_seq queue;
    let steps, next = Queue.take queue in
    bfs visited queue next steps

let main () =
  let part1 = bfs (Hashtbl.create 2024) (Queue.create ()) (0, init_floors_1) 0 in
  let part2 = bfs (Hashtbl.create 2024) (Queue.create ()) (0, init_floors_2) 0 in
  (part1 |> string_of_int, part2 |> string_of_int)

let () = Aoc.timer main
