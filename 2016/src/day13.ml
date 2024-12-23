let input = 1364

let rec popcnt c i =
  if i = 0 then c else popcnt (c + (i mod 2)) (i / 2)

let is_walkable (x,y) =
  let sum = x*x + 3*x + 2*x*y + y + y*y + input in
  popcnt 0 sum mod 2 = 0

let valid_neighbours visited (x,y) = [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
  |> List.filter (fun (x,y) -> x > -1 && y > -1)
  |> List.filter (fun pos -> pos |> Hashtbl.mem visited |> not)
  |> List.filter is_walkable

let bfs end_fn =
  let rec bfs_impl visited queue (steps, curr) =
    if end_fn steps curr then (steps, Hashtbl.length visited)
    else
      let neighbours = valid_neighbours visited curr in
      neighbours |> List.iter (fun n -> Hashtbl.add visited n true);
      neighbours |> List.to_seq |> Seq.map (fun n -> (steps+1, n)) |> Queue.add_seq queue;
      bfs_impl visited queue (Queue.take queue)
  in
  bfs_impl (Hashtbl.create 256) (Queue.create ()) (0,(1,1))

let main () =
  let part1 = bfs (fun _ pos -> pos = (31,39)) |> fst in
  let part2 = bfs (fun steps _ -> steps = 50) |> snd in
  (string_of_int part1, string_of_int part2)

let () = Aoc.timer main
