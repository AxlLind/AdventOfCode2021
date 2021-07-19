let input = 1364

let rec popcnt c i =
  if i > 0 then
    let c = c + (i mod 2) in
    popcnt c (i / 2)
  else c

let is_walkable (x,y) =
  let sum = x*x + 3*x + 2*x*y + y + y*y + input in
  (popcnt 0 sum) mod 2 = 0

let valid_neighbours visited (x,y) = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
  |> List.filter (fun (x,y) -> x > -1 && y > -1)
  |> List.filter (fun pos -> not (pos |> Hashtbl.mem visited))
  |> List.filter is_walkable

let bfs end_fn =
  let rec bfs_impl visited queue curr steps =
    if end_fn steps curr then (steps, visited |> Hashtbl.length)
    else
      let neighbours = curr |> valid_neighbours visited in
      neighbours |> List.iter (fun n -> Hashtbl.add visited n true);
      neighbours |> List.to_seq |> Seq.map (fun n -> (steps+1, n)) |> Queue.add_seq queue;
      let (steps, next) = Queue.take queue in
      bfs_impl visited queue next steps
  in
  bfs_impl (Hashtbl.create 256) (Queue.create ()) (1,1) 0

let main () =
  let part1 = bfs (fun _ pos -> pos = (31,39)) |> fst in
  let part2 = bfs (fun steps _ -> steps = 50) |> snd in
  (part1 |> string_of_int, part2 |> string_of_int)

let () = Aoc.timer main
