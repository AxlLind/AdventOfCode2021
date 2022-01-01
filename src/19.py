from aoc import main

INPUT = "#ip 3\naddi 3 16 3\nseti 1 6 5\nseti 1 8 2\nmulr 5 2 1\neqrr 1 4 1\naddr 1 3 3\naddi 3 1 3\naddr 5 0 0\naddi 2 1 2\ngtrr 2 4 1\naddr 3 1 3\nseti 2 3 3\naddi 5 1 5\ngtrr 5 4 1\naddr 1 3 3\nseti 1 8 3\nmulr 3 3 3\naddi 4 2 4\nmulr 4 4 4\nmulr 3 4 4\nmuli 4 11 4\naddi 1 6 1\nmulr 1 3 1\naddi 1 10 1\naddr 4 1 4\naddr 3 0 3\nseti 0 0 3\nsetr 3 9 1\nmulr 1 3 1\naddr 3 1 1\nmulr 3 1 1\nmuli 1 14 1\nmulr 1 3 1\naddr 4 1 4\nseti 0 4 0\nseti 0 0 3"

# Function reverse-engineered from the assembly:
# 00:  jump 17
# 01:  seti 1 6 5    ; r5 = 1
# 02:  seti 1 8 2    ; r2 = 1
# 03:  mulr 5 2 1    ; r1 = 1*1 = 1
# 04:  eqrr 1 4 1    ; r1 = 1 if r1 == r4 else 0
# 05:  addr 1 pc pc  ; if r1 == r4 { jump 7 }
# 06:  jump 8
# 07:  addr 5 0 0    ; r0 += r5
# 08:  addi 2 1 2    ; r2 += 1
# 09:  gtrr 2 4 1    ; r1 = 1 if r2 > r4 else 0
# 10:  addr pc 1 pc  ; if r2 > r4 { jump 12 }
# 11:  jump 3
# 12:  addi 5 1 5    ; r5 += 1
# 13:  gtrr 5 4 1    ; r1 = 1 if r5 > r4 else 0
# 14:  addr 1 pc pc  ; if r5 > r4 { jump 16 }
# 15:  jump 2
# 16:  HALT
# 17:  addi 4 2 4    ; r4 += 2
# 18:  mulr 4 4 4    ; r4 *= r4
# 19:  mulr pc 4 4   ; r4 *= 19
# 20:  muli 4 11 4   ; r4 *= 11, r4 = 836
# 21:  addi 1 6 1    ; r1 += 6
# 22:  mulr 1 pc 1   ; r1 *= 22
# 23:  addi 1 10 1   ; r1 += 10
# 24:  addr 4 1 4    ; r4 += r1
# 25:  jump r0
# 26:  jump 0
# 27:  setr pc 9 1   ; r1 = 27
# 28:  mulr 1 pc 1   ; r1 *= 28
# 29:  addr pc 1 1   ; r1 += 29
# 30:  mulr pc 1 1   ; r1 *= 30
# 31:  muli 1 14 1   ; r1 *= 14
# 32:  mulr 1 pc 1   ; r1 *= 32, r1 = 10550400
# 33:  addr 4 1 4    ; r4 += 10550400
# 34:  seti 0 4 0    ; r0 = 0
# 35:  jump 1

def f(n: int) -> int:
  return sum(d+n//d for d in range(1, int(n**0.5)+1) if n % d == 0)

def solve():
  lines = INPUT.split('\n')
  a, b = [int(lines[n].split(' ')[2]) for n in [22,24]]
  n = 836 + 22 * a + b
  return f(n), f(n+10550400)

main(solve)
