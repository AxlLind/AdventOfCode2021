from pathlib import Path
from z3 import Int, Solver

# TODO: Port this to Rust somehow??

inp = Path("./inputs/24.in").read_text().strip()
lines = []
for l in inp.splitlines():
    a, b = l.split(' @ ')
    x, y, z = [int(w) for w in a.split(', ')]
    dx, dy, dz = [int(w) for w in b.split(', ')]
    lines.append(([x,y,z], [dx,dy,dz]))

fx,  fy,  fz  = Int("fx"),  Int("fy"),  Int("fz")
fdx, fdy, fdz = Int("fdx"), Int("fdy"), Int("fdz")
s = Solver()
for i, ((x,y,z), (dx,dy,dz)) in enumerate(lines):
    t = Int(f"t{i}")
    s.add(t >= 0)
    s.add(x + dx * t == fx + fdx * t)
    s.add(y + dy * t == fy + fdy * t)
    s.add(z + dz * t == fz + fdz * t)
assert str(s.check()) == 'sat'

print(s.model().eval(fx + fy + fz))
