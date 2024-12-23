import time
from typing import Any, Callable, Iterable
from pathlib import Path

INPUT_DIR = (Path(__file__) / '..' / '..' / 'inputs').resolve()
AocSolution = Callable[[str], Iterable[Any]]

def main(day: str) -> Callable[[AocSolution], Callable[[],None]]:
  def decorator(solver: AocSolution) -> Callable[[],None]:
    def timer() -> None:
      indata = (INPUT_DIR / f'{day}.in').read_text()
      start = time.time()
      p1, p2 = solver(indata.rstrip('\n'))
      elapsed = int((time.time() - start) * 1000)
      print(f"Part 1: {p1}")
      print(f"Part 2: {p2}")
      print(f"Time: {elapsed}ms")
    return timer
  return decorator
