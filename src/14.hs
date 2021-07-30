import Aoc
import Data.Bits
import Data.Function
import Data.Char
import Data.List.Split (chunksOf, splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

input = "ugkiagan-"

splitRange :: [Int] -> (Int,Int) -> ([Int],[Int],[Int])
splitRange lst (i,len) = (a,b,c)
  where
    (a,tmp) = splitAt i lst
    (b,c) = splitAt len tmp

doOperation :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
doOperation (lst, i, skip) len = (newList, (i+skip+len) `mod` 256, skip+1)
  where
    (a,b,c) = splitRange (lst++lst) (i,len)
    revList = a ++ reverse b ++ c
    newList =
      let spillover = i + len - 256 in
      let part = take spillover (drop 256 revList) in
      take 256 (part ++ (drop spillover revList))

-- from day 10
knotHash :: String -> [Int]
knotHash s = chunksOf 16 lst & map (foldl xor 0)
  where
    lengths = map ord s ++ [17, 31, 73, 47, 23]
    (lst,_,_) = replicate 64 lengths & concat & foldl doOperation ([0..255],0,0)

isUsed :: [[Int]] -> (Int,Int) -> Bool
isUsed grid (x,y) = testBit byte (7 - bit)
  where
    (n, bit) = divMod y 8
    byte = grid!!x!!n

removeComponent :: Set (Int,Int) -> (Int,Int) -> Set (Int,Int)
removeComponent left (x,y) = if Set.notMember (x,y) left then left else newSet
  where
    newSet = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)] & foldl (\left n -> removeComponent left n) (Set.delete (x,y) left)

numComponents :: Set (Int,Int) -> Int
numComponents left = if Set.size left == 0 then 0 else 1 + numComponents newLeft
  where
    next = head $ Set.toList left
    newLeft = removeComponent left next

solveParts :: Int -> (Int,Int)
solveParts _ = (Set.size nodes, numComponents nodes)
  where
    grid = map (knotHash . (input++) . show) [0..127]
    nodes = [(x,y) | x <- [0..127], y <- [0..127]] & filter (isUsed grid) & Set.fromList

main = Aoc.timer solveParts
