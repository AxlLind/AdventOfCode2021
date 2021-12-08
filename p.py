import itertools

with open("input.txt") as f:
    s = f.read().strip().split("\n")

c = 0
for line in s:
    a,b = line.split(" | ")
    b = b.split(" ")
    for x in b:
        if len(x) in (2,3,4,7):
            c += 1
print(c)

m = {"acedgfb":8, "cdfbe":5, "gcdfa":2, "fbcad":3, "dab":7,
         "cefabd":9, "cdfgeb":6, "eafb":4, "cagedb":0, "ab":1}

m = {"".join(sorted(k)):v for k,v in m.items()}

ans = 0
for line in s:
    a,b = line.split(" | ")
    a = a.split(" ")
    b = b.split(" ")
    for perm in itertools.permutations("abcdefg"):
        pmap = {a:b for a,b in zip(perm,"abcdefg")}
        anew = ["".join(pmap[c] for c in x) for x in a]
        bnew = ["".join(pmap[c] for c in x) for x in b]
        if all("".join(sorted(an)) in m for an in anew):
            bnew = ["".join(sorted(x)) for x in bnew]
            ans += int("".join(str(m[x]) for x in bnew))
            break
print(ans)
