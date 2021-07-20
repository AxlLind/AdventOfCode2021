let input = "ihaygndm"

let hash_num times i =
  let rec hash_impl left h =
    if left > 0 then
      h |> Digest.string |> Digest.to_hex |> hash_impl (left-1)
    else h
  in
  hash_impl times (input ^ (string_of_int i))

let rec all_fives res i h =
  if i + 4 < String.length h then
    let found = [1;2;3;4] |> List.for_all (fun j -> h.[i] = h.[i+j]) in
    let res = if found then (h.[i]::res) else res in
    all_fives res (i+1) h
  else res

let rec hash_triple i h =
  if i < String.length h then
    let (a,b,c) = (h.[i-2], h.[i-1], h.[i]) in
    if a = b && b = c then Some a else hash_triple (i+1) h
  else None

let handle_hash_num times i =
  let h = hash_num times i in
  let c = hash_triple 2 h in
  let fives = all_fives [] 0 h in
  (c, fives)

let init_queue times =
  let queue = Queue.create () in
  for i = 0 to 999 do
    queue |> Queue.add (handle_hash_num times i)
  done;
  queue

let find_64th times =
  let q = init_queue times in
  let rec find_impl res i =
    if List.length res = 64 then List.hd res
    else (
      let (triple, fives) = Queue.take q in
      Queue.add (handle_hash_num times (i+1000)) q;
      let res = match triple with
      | Some c ->
        let valid_hashes = Queue.to_seq q
        |> Seq.filter (fun (_,fives) -> fives |> List.mem c)
        |> List.of_seq
        |> List.length in
        if valid_hashes > 0 then i::res else res
      | None -> res in
      find_impl res (i+1)
    )
  in
  find_impl [] 0

let main () =
  let part1 = 1    |> find_64th |> string_of_int in
  let part2 = 2017 |> find_64th |> string_of_int in
  (part1, part2)

let () = Aoc.timer main
