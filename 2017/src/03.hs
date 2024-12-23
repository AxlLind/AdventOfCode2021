import Aoc
import Data.Function
import qualified Data.Map as Map

type PosMap = Map.Map (Int,Int) Int
type State = ((Int,Int), (Int,Int), PosMap)

initState = ((0,0), (1,0), Map.fromList [((0,0),1)])

turn :: (Int,Int) -> (Int,Int)
turn ( 1,0) = (0,1)
turn (-1,0) = (0,-1)
turn (0, 1) = (-1,0)
turn (0,-1) = (1,0)
turn dir = dir

atCorner :: PosMap -> (Int,Int) -> Bool
atCorner m (x,y) = neighbours & filter (`Map.notMember` m) & length & (==3)
  where neighbours = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

move :: Int -> State -> State
move v ((x,y),(dx,dy),m) = (newPos, newDir, newMap)
  where
    newPos = (x+dx, y+dy)
    newDir = if atCorner m newPos then turn (dx,dy) else (dx,dy)
    newMap = Map.insert newPos v m

findPosOf :: Int -> Int -> State -> (Int,Int)
findPosOf goal prev state
  | prev+1 == goal = newPos
  | otherwise      = findPosOf goal (prev+1) (newPos, newDir, newMap)
  where (newPos, newDir, newMap) = move (prev+1) state

findPart1 :: Int -> Int
findPart1 goal = abs x + abs y
  where (x,y) = findPosOf goal 1 initState

findPart2 :: Int -> State -> Int
findPart2 goal ((x,y),(dx,dy),m)
  | newVal > goal = newVal
  | otherwise     = findPart2 goal newState
  where
    neighbours = [(1,-1),(1,0),(1,1),(0,-1),(0,1),(-1,-1),(-1,0),(-1,1)]
    newVal = neighbours & map (\(a,b) -> Map.findWithDefault 0 (x+dx+a,y+dy+b) m) & sum
    newState = move newVal ((x,y),(dx,dy),m)

solveParts :: Int -> (Int,Int)
solveParts _ = (findPart1 289326, findPart2 289326 initState)

main = Aoc.timer solveParts
