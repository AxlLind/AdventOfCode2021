import Aoc
import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

input = "..####.###.##..##....##..\n.##..#.###.##.##.###.###.\n......#..#.#.....#.....#.\n##.###.#.###.##.#.#..###.\n#..##...#.....##.#..###.#\n.#..#...####...#.....###.\n##...######.#.###..#.##..\n###..#..##.###....##.....\n.#.#####.###.#..#.#.#..#.\n#.#.##.#.##..#.##..#....#\n..#.#.#.#.#.##...#.####..\n##.##..##...#..##..#.####\n#.#..####.##.....####.##.\n..####..#.#.#.#.##..###.#\n..#.#.#.###...#.##..###..\n#.####.##..###.#####.##..\n.###.##...#.#.#.##....#.#\n#...######...#####.###.#.\n#.####.#.#..#...##.###...\n####.#.....###..###..#.#.\n..#.##.####.#######.###..\n#.##.##.#.#.....#...#...#\n###.#.###..#.#...#...##..\n##..###.#..#####.#..##..#\n#......####.#.##.#.###.##"

type Pos = (Int,Int)
data Tile = Clean | Weakened | Infected | Flagged

parseInput :: String -> Map Pos Tile
parseInput s = zip [0..] (lines s) & concatMap parseLine & Map.fromList
  where
    parseLine (x,line) = zip [0..] line & filter ((=='#') . snd) & map (\(y,_) -> ((x,y), Infected))

turn :: Bool -> Pos -> Pos
turn right (-1,0) = if right then (0,1)  else (0,-1)
turn right (1,0)  = if right then (0,-1) else (0,1)
turn right (0,-1) = if right then (-1,0) else (1,0)
turn right (0,1)  = if right then (1,0)  else (-1,0)

step :: (Pos -> Tile -> (Int, Tile, Pos)) -> (Map Pos Tile, Pos, Pos, Int) -> (Map Pos Tile, Pos, Pos, Int)
step updateFn (state, (x,y), dir, res) = (newState, (x+dx,y+dy), (dx,dy), res+dr)
  where
    (dr, tile, (dx,dy)) = Map.findWithDefault Clean (x,y) state & updateFn dir
    newState = Map.insert (x,y) tile state

updatePart1 :: Pos -> Tile -> (Int, Tile, Pos)
updatePart1 dir Clean    = (1, Infected, turn False dir)
updatePart1 dir Infected = (0, Clean,    turn True dir)
updatePart1 _ _ = error "invalid state"

updatePart2 :: Pos -> Tile -> (Int, Tile, Pos)
updatePart2 dir Clean    = (0, Weakened, turn False dir)
updatePart2 dir Weakened = (1, Infected, dir)
updatePart2 dir Infected = (0, Flagged,  turn True dir)
updatePart2 dir Flagged  = (0, Clean,    turn True dir & turn True)

runSteps :: Map Pos Tile -> (Pos -> Tile -> (Int, Tile, Pos)) -> Int -> Int
runSteps initState updateFn n = res
  where (_,_,_,res) = iterate (step updateFn) (initState, (12,12), (-1,0), 0) !! n

solveParts :: Int -> (Int,Int)
solveParts _ = (runSteps initState updatePart1 10000, runSteps initState updatePart2 10000000)
  where initState = parseInput input

main = Aoc.timer solveParts
