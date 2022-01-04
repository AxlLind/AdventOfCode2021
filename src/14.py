import aoc

@aoc.main('14')
def main(indata: str):
  r = int(indata)
  res = [int(c) for c in str(r)]
  e1,e2,recipes = 0,1,[3,7]
  while recipes[-len(res):] != res:
    x = recipes[e1] + recipes[e2]
    if x > 9:
      recipes.append(1)
      if recipes[-len(res):] == res:
        break
    recipes.append(x % 10)
    e1 = (e1 + recipes[e1] + 1) % len(recipes)
    e2 = (e2 + recipes[e2] + 1) % len(recipes)
  return ''.join(str(i) for i in recipes[r:r+10]),len(recipes) - len(res)

if __name__ == "__main__":
  main()
