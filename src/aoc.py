import time
from typing import Any, Callable

def main(solver: Callable[[], tuple[Any,Any]]):
  start = time.time()
  p1, p2 = solver()
  elapsed = int((time.time() - start) * 1000)
  print(f"Part 1: {p1}")
  print(f"Part 2: {p2}")
  print(f"Time: {elapsed}ms")
