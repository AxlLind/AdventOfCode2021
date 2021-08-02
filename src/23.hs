import Aoc
import Data.Function
import Text.Read
import Data.Vector (Vector, (!), (//), (!?))
import qualified Data.Vector as Vec

input = "set b 67\nset c b\njnz a 2\njnz 1 5\nmul b 100\nsub b -100000\nset c b\nsub c -17000\nset f 1\nset d 2\nset e 2\nset g d\nmul g e\nsub g b\njnz g 2\nset f 0\nsub e -1\nset g e\nsub g b\njnz g -8\nsub d -1\nset g d\nsub g b\njnz g -13\njnz f 2\nsub h -1\nset g b\nsub g c\njnz g 2\njnz 1 3\nsub b -17\njnz 1 -23"

data Src = Reg Int | Val Int

data Op = Set Int Src
        | Sub Int Src
        | Mul Int Src
        | Jnz Src Src

parseInput :: String -> Vector Op
parseInput s = map parseLine (lines s) & Vec.fromList
  where
    regToInt "a" = 0
    regToInt "b" = 1
    regToInt "c" = 2
    regToInt "d" = 3
    regToInt "e" = 4
    regToInt "f" = 5
    regToInt "g" = 6
    regToInt "h" = 7
    parseSrc s = case readMaybe s of
      Just n -> Val n
      Nothing -> Reg (regToInt s)
    parseLine s = case words s of
      ["set",x,y] -> Set (regToInt x) (parseSrc y)
      ["sub",x,y] -> Sub (regToInt x) (parseSrc y)
      ["mul",x,y] -> Mul (regToInt x) (parseSrc y)
      ["jnz",x,y] -> Jnz (parseSrc x) (parseSrc y)

fetchSrc :: Vector Int -> Src -> Int
fetchSrc _    (Val n) = n
fetchSrc regs (Reg n) = regs!n

execOp :: Vector Int -> Op -> (Int, Int, Vector Int)
execOp regs (Set x y) = (0, 1, regs // [(x, fetchSrc regs y)])
execOp regs (Sub x y) = (0, 1, regs // [(x, regs!x - fetchSrc regs y)])
execOp regs (Mul x y) = (1, 1, regs // [(x, regs!x * fetchSrc regs y)])
execOp regs (Jnz x y) = (0, if fetchSrc regs x /= 0 then fetchSrc regs y else 1, regs)

part1 :: Vector Op -> Vector Int -> Int -> Int
part1 ops regs ip = case ops !? ip of
  Just op ->
    let (dres, offset, newRegs) = execOp regs op in
    dres + part1 ops newRegs (ip+offset)
  Nothing -> 0

isPrime :: Int -> Bool
isPrime k = k > 1 && null [ x | x <- [2..isqrt k], k `mod` x == 0]
  where isqrt = floor . sqrt . fromIntegral

part2 :: Op -> Int
part2 (Set _ (Val n)) = filter (not . isPrime . (+ start)) [0,17..17000] & length
  where start = n * 100 + 100000

solveParts :: Int -> (Int,Int)
solveParts _ = (part1 ops (Vec.fromList [0,0,0,0,0,0,0,0]) 0, part2 (Vec.head ops))
  where
    ops = parseInput input

main = Aoc.timer solveParts
