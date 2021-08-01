import Aoc
import Data.Function

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 v lst = v:lst
insertAt i v (x:xs) = x:insertAt (i-1) v xs

findAfter :: Int -> [Int] -> Int
findAfter n (a:xs) = if a == n then head xs else findAfter n xs

spinLock :: Int -> Int -> Int -> [Int] -> [Int]
spinLock goal n i lst = if goal == n then lst else spinLock goal (n+1) (idx+1) (insertAt (idx+1) (n+1) lst)
  where idx = (i + 335) `rem` (n+1)

solveParts :: Int -> (Int,Int)
solveParts _ = (spinLock 2017 0 0 [0] & findAfter 2017, (spinLock 50000000 0 0 [0])!!1)

main = Aoc.timer solveParts
