from aoc import main

INPUT = "#ip 3\naddi 3 16 3\nseti 1 6 5\nseti 1 8 2\nmulr 5 2 1\neqrr 1 4 1\naddr 1 3 3\naddi 3 1 3\naddr 5 0 0\naddi 2 1 2\ngtrr 2 4 1\naddr 3 1 3\nseti 2 3 3\naddi 5 1 5\ngtrr 5 4 1\naddr 1 3 3\nseti 1 8 3\nmulr 3 3 3\naddi 4 2 4\nmulr 4 4 4\nmulr 3 4 4\nmuli 4 11 4\naddi 1 6 1\nmulr 1 3 1\naddi 1 10 1\naddr 4 1 4\naddr 3 0 3\nseti 0 0 3\nsetr 3 9 1\nmulr 1 3 1\naddr 3 1 1\nmulr 3 1 1\nmuli 1 14 1\nmulr 1 3 1\naddr 4 1 4\nseti 0 4 0\nseti 0 0 3"

def run_inst(regs,inst):
  op,a,b,c = inst
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

def part1(insts: list[list[int]], ipreg: int) -> int:
  regs,pc = [0,0,0,0,0,0],0
  while 0 <= pc < len(insts):
    regs[ipreg] = pc
    regs = run_inst(regs,insts[pc])
    pc = regs[ipreg]+1
  return regs[0]

def part2(insts: list[list[int]]) -> int:
  a, b = insts[21][2], insts[23][2]
  n = 836 + 22 * a + b + 10550400
  return sum(d + n // d for d in range(1, int(n**0.5)+1) if n % d == 0)

def solve():
  insts = []
  for l in INPUT.split('\n'):
    words = l.split(' ')
    ops = [int(i) for i in words[1:]]
    match words[0]:
      case "#ip":  ipreg = ops[0]
      case "addr": insts.append([0,  *ops])
      case "addi": insts.append([1,  *ops])
      case "mulr": insts.append([2,  *ops])
      case "muli": insts.append([3,  *ops])
      case "banr": insts.append([4,  *ops])
      case "bani": insts.append([5,  *ops])
      case "borr": insts.append([6,  *ops])
      case "bori": insts.append([7,  *ops])
      case "setr": insts.append([8,  *ops])
      case "seti": insts.append([9,  *ops])
      case "gtir": insts.append([10, *ops])
      case "gtri": insts.append([11, *ops])
      case "gtrr": insts.append([12, *ops])
      case "eqir": insts.append([13, *ops])
      case "eqri": insts.append([14, *ops])
      case "eqrr": insts.append([15, *ops])
      case _: assert False
  return part1(insts, ipreg), part2(insts)

main(solve)
