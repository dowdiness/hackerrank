module Algorithms.Warmup.ComparetheTriplets where

-- Compare the Triplets
-- https://www.hackerrank.com/challenges/compare-the-triplets

comp :: Int -> Int -> (Int,Int)
comp a b
  | a > b = (1,0)
  | a == b = (0,0)
  | a < b = (0,1)

solve :: [[Int]] -> [Int]
solve [xs, ys] = [f, s]
  where
    (f, s) = foldl (\(x,y) (xx, yy) -> (x + xx, y + yy)) (0,0) tuples
    tuples = zipWith comp xs ys

main :: IO()
main = interact $
  unwords .
  map show .
  solve .
  map (map (read::String -> Int) . words) .
  lines
