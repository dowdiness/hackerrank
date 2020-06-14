module Algorithms.Implementation.DivisibleSumPairs where

-- Divisible Sum Pairs
-- https://www.hackerrank.com/challenges/divisible-sum-pairs

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs


solve :: [Int] -> Int
solve (x:xs) =
  length $
  filter (\z -> z `mod` x == 0) $
  map (\[y,z] -> y + z) $
  comb 2 xs

main :: IO ()
main = interact $ show . solve . map read . tail . words
