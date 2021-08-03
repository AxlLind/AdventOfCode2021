import Aoc
import Data.Function
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

input = "Begin in state A.\nPerform a diagnostic checksum after 12481997 steps.\n\nIn state A:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state C.\n\nIn state B:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state D.\n\nIn state C:\n  If the current value is 0:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state B.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the left.\n    - Continue with state E.\n\nIn state D:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A.\n  If the current value is 1:\n    - Write the value 0.\n    - Move one slot to the right.\n    - Continue with state B.\n\nIn state E:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state F.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the left.\n    - Continue with state C.\n\nIn state F:\n  If the current value is 0:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state D.\n  If the current value is 1:\n    - Write the value 1.\n    - Move one slot to the right.\n    - Continue with state A."

parseInput :: String -> Map Char [(Int,Int,Char)]
parseInput s = splitOn "\n\n" s & tail & map parseState & Map.fromList
  where
    parseInstruction (a:b:c:_) =
      let v = read a in
      let move = if b == "left" then -1 else 1 in
      let state = head c in
      (v, move, state)
    parseState s =
      let ws = filter (\c -> c /= '.' && c /= ':') s & lines & map (last . words)  in
      let state = head (head ws) in
      (state, [parseInstruction $ drop 2 ws, parseInstruction $ drop 6 ws])

runTuringMachine :: Map Char [(Int,Int,Char)] -> Map Int Int -> Int -> Int -> Char -> Int
runTuringMachine _ tape 0 _ _ = Map.elems tape & sum
runTuringMachine instructions tape left i state = runTuringMachine instructions (Map.insert i v tape) (left-1) (i+move) newState
  where
    currVal = Map.findWithDefault 0 i tape
    (v, move, newState) = Map.findWithDefault [] state instructions !! currVal

solveParts :: Int -> (Int,String)
solveParts _ = (runTuringMachine instructions Map.empty 12481997 0 'A', "25/25")
  where
    instructions = parseInput input

main = Aoc.timer solveParts
