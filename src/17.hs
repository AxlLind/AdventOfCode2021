{-# LANGUAGE BangPatterns #-}
import Aoc
import Data.Function

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 v lst = v:lst
insertAt i v (x:xs) = x:insertAt (i-1) v xs
insertAt i _ [] = error "out of bounds"

findAfter :: Int -> [Int] -> Int
findAfter n (a:xs) = if a == n then head xs else findAfter n xs
findAfter _ [] = error "out of bounds"

part1 :: Int -> Int
part1 times = foldl next ([0],0) [1..times] & fst & findAfter 2017
  where
    next (lst,i) n =
      let idx = 1 + (i + 335) `rem` n in
      (insertAt idx n lst, idx)

part2 :: Int -> Int
part2 times = foldl next (0,0) [1..times] & fst
  where
    next (res,i) n =
      let !idx = 1 + (i + 335) `rem` n in
      let !newRes = if idx == 1 then n else res in
      (newRes, idx)

solveParts :: Int -> (Int,Int)
solveParts _ = (part1 2017, part2 50000000)

main = Aoc.timer solveParts
