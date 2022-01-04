import aoc
from itertools import product

DR,DC = [0,1,0,-1],[1,0,-1,0]

def move_cart(crosses: dict[tuple[int,int],str], cart: list[int]) -> list[int]:
  r,c,d,t = cart
  match crosses.get((r,c), " ")[0]:
    case '+':
      match t:
        case 0: d = [3,0,1,2][d]
        case 2: d = [1,2,3,0][d]
      t = (t+1)%3
    case '/':  d = [3,2,1,0][d]
    case '\\': d = [1,0,3,2][d]
  return [r+DR[d],c+DC[d],d,t]

def step(crosses: dict[tuple[int,int],str], carts: list[list[int]]) -> list[int]:
  carts.sort()
  crashes = set()
  for i in range(len(carts)):
    r,c,d,t = move_cart(crosses, carts[i])
    for j in range(len(carts)):
      if carts[j][0] == r and carts[j][1] == c:
        crashes.add(i)
        crashes.add(j)
    carts[i] = [r,c,d,t]
  return sorted(crashes)

@aoc.main('13')
def main(indata: str):
  grid, crosses, carts = indata.split('\n'), {}, []
  for r,c in product(range(len(grid)), range(len(grid[0]))):
    match grid[r][c]:
      case '>': carts.append([r,c,0,0])
      case 'v': carts.append([r,c,1,0])
      case '<': carts.append([r,c,2,0])
      case '^': carts.append([r,c,3,0])
      case '+': crosses[r,c] = '+',
      case '/': crosses[r,c] = '/',
      case '\\': crosses[r,c] = '\\',
  r1,c1 = None,None
  while len(carts) > 1:
    for i in reversed(step(crosses, carts)):
      if not r1:
        r1,c1 = carts[i][:2]
      carts.pop(i)
  r2,c2 = carts[0][:2]
  return f"{c1},{r1}", f"{c2},{r2}"

if __name__ == "__main__":
  main()
