import Aoc
import Data.Function
import Data.List.Split

input = "0: 3\n1: 2\n2: 4\n4: 4\n6: 5\n8: 8\n10: 6\n12: 6\n14: 6\n16: 6\n18: 8\n20: 8\n22: 12\n24: 10\n26: 9\n28: 8\n30: 8\n32: 12\n34: 12\n36: 12\n38: 12\n40: 8\n42: 12\n44: 14\n46: 14\n48: 10\n50: 12\n52: 12\n54: 14\n56: 14\n58: 14\n62: 12\n64: 14\n66: 14\n68: 14\n70: 12\n74: 14\n76: 14\n78: 14\n80: 18\n82: 17\n84: 30\n88: 14"

parseInput :: String -> [(Int,Int)]
parseInput input = map parseLine (lines input)
  where
    parseLine s = case splitOn ": " s & map read of
      [a,b] -> (a,b)
      _ -> error "invalid input"

caught :: Int -> (Int,Int) -> Bool
caught offset (layer,depth) = (layer + offset) `rem` (depth * 2 - 2) == 0

solveParts :: Int -> (Int,Int)
solveParts _ = (part1, part2)
  where
    layers = parseInput input
    part1 = layers & filter (caught 0) & map (\(layer,depth) -> layer * depth) & sum
    part2 = [0..] & filter (\o -> all (not . caught o) layers) & head

main = Aoc.timer solveParts
