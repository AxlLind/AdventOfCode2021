import Aoc
import Data.List.Split
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set

input = "42/37\n28/28\n29/25\n45/8\n35/23\n49/20\n44/4\n15/33\n14/19\n31/44\n39/14\n25/17\n34/34\n38/42\n8/42\n15/28\n0/7\n49/12\n18/36\n45/45\n28/7\n30/43\n23/41\n0/35\n18/9\n3/31\n20/31\n10/40\n0/22\n1/23\n20/47\n38/36\n15/8\n34/32\n30/30\n30/44\n19/28\n46/15\n34/50\n40/20\n27/39\n3/14\n43/45\n50/42\n1/33\n6/39\n46/44\n22/35\n15/20\n43/31\n23/23\n19/27\n47/15\n43/43\n25/36\n26/38\n1/10"

parseInput :: String -> Set (Int,Int)
parseInput = Set.fromList . map parseLine . lines
  where
    parseLine s = case splitOn "/" s & map read of
      [a,b] -> (a,b)
      _ -> error "invalid input"

maxStrength :: Set (Int,Int) -> ((Int,Int) -> (Int,Int) -> (Int,Int)) -> Int -> Int -> Int -> (Int,Int)
maxStrength ports maxFn len strength x = availablePorts & map childStrength & foldl maxFn (len,strength)
  where
    availablePorts = Set.filter (\(a,b) -> a == x || b == x) ports & Set.toList
    childStrength (a,b) = maxStrength (Set.delete (a,b) ports) maxFn (len+1) (strength+a+b) (if a == x then b else a)

solveParts :: Int -> (Int,Int)
solveParts _ = (part1, part2)
  where
    ports = parseInput input
    (_,part1) = maxStrength ports (\a b -> if snd a > snd b then a else b) 0 0 0
    (_,part2) = maxStrength ports max 0 0 0

main = Aoc.timer solveParts
