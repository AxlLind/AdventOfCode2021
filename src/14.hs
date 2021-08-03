import Aoc
import Data.Bits
import Data.Function
import Data.Char
import Data.List.Split (chunksOf, splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

input = "ugkiagan-"

performKnot :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
performKnot (lst, i, skip) len = (newList, (i+skip+len) `mod` 256, skip+1)
  where
    (a,tmp) = splitAt i lst
    (b,c) = splitAt len (tmp++lst)
    revList = a ++ reverse b ++ c
    newList =
      let spillover = i + len - 256 in
      let part = take spillover (drop 256 revList) in
      take 256 (part ++ drop spillover revList)

-- from day 10
knotHash :: String -> [Int]
knotHash s = chunksOf 16 lst & map (foldl xor 0)
  where
    lengths = map ord s ++ [17, 31, 73, 47, 23]
    (lst,_,_) = concatMap (const lengths) [1..64] & foldl performKnot ([0..255],0,0)

isUsed :: [[Int]] -> (Int,Int) -> Bool
isUsed grid (x,y) = testBit byte (7 - bit)
  where
    (n, bit) = divMod y 8
    byte = grid!!x!!n

removeComponent :: Set (Int,Int) -> (Int,Int) -> Set (Int,Int)
removeComponent nodes (x,y)
  | Set.member (x,y) nodes = foldl removeComponent (Set.delete (x,y) nodes) neighbours
  | otherwise              = nodes
  where
    neighbours = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

numComponents :: Set (Int,Int) -> Int
numComponents nodes
  | Set.size nodes == 0 = 0
  | otherwise           = 1 + numComponents newLeft
  where
    next = head $ Set.toList nodes
    newLeft = removeComponent nodes next

solveParts :: Int -> (Int,Int)
solveParts _ = (Set.size nodes, numComponents nodes)
  where
    grid = map (knotHash . (input++) . show) [0..127]
    nodes = Set.fromList [(x,y) | x <- [0..127], y <- [0..127], isUsed grid (x,y)]

main = Aoc.timer solveParts
