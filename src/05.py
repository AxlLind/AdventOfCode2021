import aoc

def react(init: str, c: str = '') -> int:
  s = [x for x in init if x.lower() != c]
  for i in range(len(s)-1, -1, -1):
    if i+1 < len(s) and s[i] != s[i+1] and s[i].upper() == s[i+1].upper():
      s.pop(i)
      s.pop(i)
  return len(s)

@aoc.main('05')
def main(indata: str) -> tuple[int,int]:
  part2 = min(react(indata, c) for c in "abcdefghijklmnopqrstuvxyz")
  return react(indata), part2

if __name__ == "__main__":
  main()
