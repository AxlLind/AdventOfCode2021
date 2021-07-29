module Aoc where
import Data.Time

elapsedMs :: UTCTime -> IO Int
elapsedMs t0 = do
  t1 <- getCurrentTime
  let dt = nominalDiffTimeToSeconds $ diffUTCTime t1 t0
  return $ (round $ dt * 1000000) `div` 1000

timer :: (Show a, Show b) => (Int -> (a,b)) -> IO()
timer fn = do
  t0 <- getCurrentTime
  let (part1, part2) = fn 0
  ms <- elapsedMs t0
  putStrLn $ "Part one: " ++ show part1
  putStrLn $ "Part two: " ++ show part2
  putStrLn $ "Time: " ++ show ms ++ "ms"
