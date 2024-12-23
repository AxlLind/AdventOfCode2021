import aoc
import re

def run_inst(regs,op,a,b,c):
  match op:
    case 0:  regs[c] = regs[a] + regs[b]
    case 1:  regs[c] = regs[a] + b
    case 2:  regs[c] = regs[a] * regs[b]
    case 3:  regs[c] = regs[a] * b
    case 4:  regs[c] = regs[a] & regs[b]
    case 5:  regs[c] = regs[a] & b
    case 6:  regs[c] = regs[a] | regs[b]
    case 7:  regs[c] = regs[a] | b
    case 8:  regs[c] = regs[a]
    case 9:  regs[c] = a
    case 10: regs[c] = int(a > regs[b])
    case 11: regs[c] = int(regs[a] > b)
    case 12: regs[c] = int(regs[a] > regs[b])
    case 13: regs[c] = int(a == regs[b])
    case 14: regs[c] = int(regs[a] == b)
    case 15: regs[c] = int(regs[a] == regs[b])
  return regs

def valid_opcodes(before,code,after):
  _,a,b,c = code
  valid = set()
  for op in range(0,16):
    if run_inst(before[:],op,a,b,c) == after:
      valid.add(op)
  return valid

@aoc.main('16')
def main(indata: str):
  lines, rest = indata.split("\n\n\n\n")
  observations = []
  for s in lines.split("\n\n"):
    nums = [int(i) for i in re.findall("\d+", s)]
    assert len(nums) == 12
    observations.append((nums[:4], nums[4:8], nums[8:]))
  program = [int(i) for i in re.findall("\d+", rest)]

  p1, possible_codes = 0, [set(range(0,16)) for _ in range(0,16)]
  for before,inst,after in observations:
    valid = valid_opcodes(before,inst,after)
    p1 += len(valid) >= 3
    for op in range(0,16):
      if op not in valid and inst[0] in possible_codes[op]:
        possible_codes[op].remove(inst[0])
  while any(len(codes) != 1 for codes in possible_codes):
    for op in range(0,16):
      if len(possible_codes[op]) > 1:
        continue
      c = list(possible_codes[op])[0]
      for op2 in range(0,16):
        if op2 != op and c in possible_codes[op2]:
          possible_codes[op2].remove(c)
  code_map = [0]*16
  for i,codes in enumerate(possible_codes):
    code_map[list(codes)[0]] = i

  regs = [0,0,0,0]
  for i in range(0,len(program)//4):
    op,a,b,c = program[i*4:i*4+4]
    regs = run_inst(regs,code_map[op],a,b,c)
  return p1, regs[0]

if __name__ == "__main__":
  main()
