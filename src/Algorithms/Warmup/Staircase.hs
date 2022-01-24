module Algorithms.Warmup.Staircase where

-- Staircase
-- https://www.hackerrank.com/challenges/staircase

staircase n = map (\m -> empty (n - m) ++ sharp m) [1..n]
  where
    empty m = concat $ replicate m " "
    sharp m = concat $ replicate m "#"

main :: IO()
main = do
    n <- readLn :: IO Int
    putStrLn $ unlines $ staircase n
