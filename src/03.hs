import Data.Function
import qualified Data.Map as M

type PosMap = M.Map (Int,Int) Int
type State = ((Int,Int), (Int,Int), PosMap)

neighbours = [(1,-1),(1,0),(1,1),(0,-1),(0,1),(-1,-1),(-1,0),(-1,1)]
initState = ((0,0), (1,0), (M.fromList [((0,0),1)]))

turn :: (Int,Int) -> (Int,Int)
turn (1,0)  = (0,1)
turn (-1,0) = (0,-1)
turn (0,1)  = (-1,0)
turn (0,-1) = (1,0)
turn pos = pos

atCorner :: PosMap -> (Int,Int) -> Bool
atCorner m (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] & filter (\pos -> M.notMember pos m) & length & (==3)

move :: Int -> State -> State
move v (x,y) (dx,dy) m = (newPos, newDir, newMap)
  where
  newPos = (x+dx, y+dy)
  newDir = if atCorner m newPos then turn (dx,dy) else (dx,dy)
  newMap = M.insert newPos v m

findPosOf :: Int -> Int -> State -> (Int,Int)
findPosOf goal prev state = if prev+1 == goal then newPos else findPosOf goal (prev+1) (newPos, newDir, newMap)
  where
  (newPos, newDir, newMap) = move (prev+1) state

findPart1 :: Int -> Int
findPart1 goal = (abs x) + (abs y)
  where (x,y) = findPosOf goal 1 initState

findPart2 :: Int -> (Int,Int) -> (Int,Int) -> PosMap -> Int
findPart2 goal ((x,y),(dx,dy),m) = if newVal > goal then newVal else findPart2 goal (newPos, newDir, newMap)
  where
  newVal = neighbours & map (\(a,b) -> M.findWithDefault 0 (x+dx+a,y+dy+b) m) & sum
  (newPos, newDir, newMap) = move newVal ((x,y),(dx,dy),m)

main = do
  findPart1 289326 & show & ("Part 1: " ++) & putStrLn
  findPart2 289326 initState & show & ("Part 2: " ++) & putStrLn
