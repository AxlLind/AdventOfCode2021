from pathlib import Path
from z3 import If, Int, Solver

inp = Path("./inputs/24.in").read_text().strip()

def parse(inp):
    lines = []
    for l in inp.splitlines():
        a, b = l.split(' @ ')
        x, y, z = [int(w) for w in a.split(', ')]
        dx, dy, dz = [int(w) for w in b.split(', ')]
        lines.append(((x,y,z),(dx,dy,dz)))
    return lines

def part2(lines):
    fx,fy,fz,opt = Int("fx"), Int("fy"), Int("fz"), Solver()
    fdx,fdy,fdz = Int("fdx"), Int("fdy"), Int("fdz")
    for i, ((x,y,z), (dx,dy,dz)) in enumerate(lines):
        t = Int(f"t{i}")
        opt.add(t >= 0)
        opt.add(x + dx * t == fx + fdx * t)
        opt.add(y + dy * t == fy + fdy * t)
        opt.add(z + dz * t == fz + fdz * t)
    assert str(opt.check()) == 'sat'
    return opt.model().eval(fx + fy + fz)

lines = parse(inp)
print(part2(lines))
