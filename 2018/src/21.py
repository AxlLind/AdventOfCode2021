import aoc

# Function reverse-engineered by hand
# 00: seti 123 0 3      ;
# 01: bani 3 456 3      ;
# 02: eqri 3 72 3       ;
# 03: addr 3 5 pc       ;
# 04: seti 0 0 pc       ;
# 05: seti 0 5 3        ; r3 = 0
# 06: bori 3 65536 2    ; r2 = r3 | 65536
# 07: seti <N> 1 3      ; r3 = <N>
# 08: bani 2 255 1      ; r1 = r2 & 0xff
# 09: addr 3 1 3        ; r3 += r1
# 10: bani 3 16777215 3 ; r3 &= 0xffffff
# 11: muli 3 65899 3    ; r3 *= 65899
# 12: bani 3 16777215 3 ; r3 &= 0xffffff
# 13: gtir 256 2 1      ; if 256 > r2 {
# 14: addr 1 pc pc      ;   jump 28
# 15: addi 5 1 pc       ; }
# 16: seti 27 7 pc      ;
# 17: seti 0 2 1        ; r1 = 0
# 18: addi 1 1 4        ; r4 = r1+1
# 19: muli 4 256 4      ; r4 *= 256
# 20: gtrr 4 2 4        ; if (r1+1)*256 < r2 {
# 21: addr 4 pc pc      ;   jump 26
# 22: addi 5 1 pc       ; }
# 23: seti 25 1 pc      ;
# 24: addi 1 1 1        ; r1 += 1
# 25: seti 17 0 pc      ; jump 18
# 26: setr 1 7 2        ; r2 = r1
# 27: seti 7 2 pc       ; jump 8
# 28: eqrr 3 0 1        ;
# 29: addr 1 5 pc       ; if r3 == r0 { HALT }
# 30: seti 5 5 pc       ;

def f(n: int, r2: int, r3: int) -> tuple[int,int,int]:
  while True:
    r3 = (((r3 + (r2 & 0xff)) & 0xffffff) * 65899)  & 0xffffff
    if r2 < 256:
      break
    r2 //= 256
  return r3 | 0x10000, n, r3

@aoc.main('21')
def main(indata: str) -> tuple[int,int]:
  n = int(indata.split('\n')[8].split(' ')[1])
  seen, r2, r3 = dict[int,int](), 65536, n
  while True:
    r2,r3,res = f(n,r2,r3)
    if res in seen:
      break
    seen[res] = len(seen)
  _, p1 = min((v,k) for k,v in seen.items())
  _, p2 = max((v,k) for k,v in seen.items())
  return p1,p2

if __name__ == "__main__":
  main()
