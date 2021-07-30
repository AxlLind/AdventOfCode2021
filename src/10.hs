import Aoc
import Data.Function
import Data.Char
import Numeric (showHex)
import Data.List.Split (chunksOf, splitOn)
import Data.Bits (xor)

input = "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12"

toHex :: Int -> String
toHex i = (if i < 0x10 then "0" else "") ++ showHex i ""

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

knotHash :: String -> String
knotHash s = chunksOf 16 lst & map (toHex . foldl xor 0) & concat
  where
    lengths = map ord s ++ [17, 31, 73, 47, 23]
    (lst,_,_) = replicate 64 lengths & concat & foldl doOperation ([0..255],0,0)

solveParts :: Int -> (Int,String)
solveParts _ = (part1, part2)
  where
    inputPart1 = splitOn "," input & map read
    (lst,_,_) = foldl doOperation ([0..255],0,0) inputPart1
    part1 = (head lst) * (head (tail lst))
    part2 = knotHash input

main = Aoc.timer solveParts
