import Aoc
import Data.Function
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as Vec
import Data.Map (Map)
import qualified Data.Map as Map

input = "4 1 15 12 0 9 9 5 5 8 7 3 14 5 12 3"

moveMaximum :: Vector Int -> Vector Int
moveMaximum v = v // updates
  where
    i = Vec.maxIndex v
    idx j = mod (i+j) (Vec.length v)
    updates = (i,0):[(idx j, (v ! idx j) + 1) | j <- [1..(v ! i)]]

solvePart1 :: Map (Vector Int) Int -> Int -> Vector Int -> (Int -> Int -> Int) -> Int
solvePart1 seen c v answerFn = case Map.lookup v seen of
  Just x -> answerFn x c
  Nothing -> solvePart1 (Map.insert v c seen) (c+1) (moveMaximum v) answerFn

solveParts :: Int -> (Int,Int)
solveParts _ = (solverFn (\_ y -> y), solverFn (\x y -> y - x))
  where solverFn = words input & map read & Vec.fromList & solvePart1 Map.empty 0

main = Aoc.timer solveParts
