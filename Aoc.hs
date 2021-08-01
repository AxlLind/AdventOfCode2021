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
  let (a,b) = fn 0
  putStrLn $ "Part one: " ++ show a
  putStrLn $ "Part one: " ++ show b
  ms <- elapsedMs t0
  putStrLn $ "Time: " ++ show ms ++ "ms"
