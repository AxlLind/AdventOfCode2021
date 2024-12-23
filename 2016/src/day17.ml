let input = "bwnlcvfs"

let valid_neighbours ((x,y),s) =
  let h = s |> Digest.string |> Digest.to_hex in
  [ x, y-1, "U", h.[0];
    x, y+1, "D", h.[1];
    x-1, y, "L", h.[2];
    x+1, y, "R", h.[3] ]
  |> List.filter (fun (x,y,_,_) -> -1 < x && x < 4 && -1 < y && y < 4)
  |> List.filter (fun (_,_,_,c) -> String.contains "bcdef" c)
  |> List.map (fun (x,y,d,_) -> ((x,y),s ^ d))

let rec bfs_part_1 queue curr =
  if fst curr = (3,3) then snd curr
  else
    let neighbours = valid_neighbours curr in
    neighbours |> List.to_seq |> Queue.add_seq queue;
    bfs_part_1 queue (Queue.take queue)

let rec bfs_part_2 longest queue curr =
  let longest =
    if fst curr = (3,3) then
      let (l1,l2) = (String.length longest, String.length (snd curr)) in
      if l1 < l2 then snd curr else longest
    else (
      curr |> valid_neighbours |> List.to_seq |> Queue.add_seq queue;
      longest
    )
  in
  match Queue.take_opt queue with
  | Some next -> bfs_part_2 longest queue next
  | None -> longest

let main () =
  let part1 = bfs_part_1 (Queue.create ()) ((0,0),input) in
  let part2 = bfs_part_2 "" (Queue.create ()) ((0,0),input) |> String.length in
  (part1, part2 - 8 |> string_of_int)

let () = Aoc.timer main
