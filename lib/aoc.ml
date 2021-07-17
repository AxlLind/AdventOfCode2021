open Format

let timer f =
  let t0 = Sys.time() in
  let _ = f () in
  let elapsed_ms = (Sys.time() -. t0) *. 1000.0 in
  printf "Time: %.3fms\n" elapsed_ms
