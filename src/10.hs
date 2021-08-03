import Aoc
import Data.Function
import Data.Char
import Numeric (showHex)
import Data.List.Split (chunksOf, splitOn)
import Data.Bits (xor)

input = "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12"

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

knotHash :: String -> String
knotHash s = chunksOf 16 lst & concatMap (toHex . foldl xor 0)
  where
    lengths = map ord s ++ [17, 31, 73, 47, 23]
    (lst,_,_) = concatMap (const lengths) [1..64] & foldl performKnot ([0..255],0,0)
    toHex i = (if i < 0x10 then "0" else "") ++ showHex i ""

solveParts :: Int -> (Int,String)
solveParts _ = (part1, knotHash input)
  where
    (lst,_,_) = splitOn "," input & map read & foldl performKnot ([0..255],0,0)
    part1 = head lst * lst!!1

main = Aoc.timer solveParts
