module Algorithms.Warmup.DiagonalDifference where

-- Diagonal Difference
-- https://www.hackerrank.com/challenges/diagonal-difference

import           Control.Monad

getList :: Read a => IO [a]
getList = map read . words <$> getLine

solve::Int -> [Int] -> Int
solve x xs = xs !! x

main :: IO ()
main = do
  n <- read <$> getLine
  list <- replicateM n (getList::IO [Int])
  let
    pr = sum $ zipWith solve [0..] list
    sn = sum $ zipWith solve [n-1, n-2..] list
  print (abs $ pr - sn)
