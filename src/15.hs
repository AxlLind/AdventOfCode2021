import Aoc
import Data.Bits

input = (591, 393)

judgement :: Int -> Int -> Int
judgement i j
  | (.&.) (xor i j) 0xffff == 0 = 1
  | otherwise                   = 0

nextVal :: Int -> Int -> Int -> Int
nextVal i d factor
  | next `rem` d == 0 = next
  | otherwise         = nextVal next d factor
  where next = (i * factor) `rem` 2147483647

numAccepted :: (Int,Int) -> (Int,Int) -> Int -> Int -> Int
numAccepted _ _ 0 c = c
numAccepted (d1,d2) (i,j) round c = numAccepted (d1,d2) nextValues (round-1) (c + judgement i j)
  where nextValues = (nextVal i d1 16807, nextVal j d2 48271)

solveParts :: Int -> (Int,Int)
solveParts _ = (numAccepted (1,1) input 40000000 0, numAccepted (4,8) input 5000000 0)

main = Aoc.timer solveParts
