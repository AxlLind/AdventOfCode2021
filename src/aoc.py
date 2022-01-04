import time
from typing import Any, Callable, Iterable
from pathlib import Path

INPUT_DIR = (Path(__file__) / '..' / '..' / 'inputs').resolve()

def main(day: str) -> Callable[[Callable[[str], Iterable[Any]]],Callable[[],None]]:
  def decorator(solver: Callable[[str], Iterable[Any]]) -> Callable[[],None]:
    def timer():
      with open(INPUT_DIR / f'{day}.in', 'r') as f:
        indata = f.read().strip('\n')
      start = time.time()
      p1, p2 = solver(indata)
      elapsed = int((time.time() - start) * 1000)
      print(f"Part 1: {p1}")
      print(f"Part 2: {p2}")
      print(f"Time: {elapsed}ms")
    return timer
  return decorator
