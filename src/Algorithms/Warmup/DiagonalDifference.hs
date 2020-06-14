module Algorithms.Warmup.DiagonalDifference where

-- Diagonal Difference
-- https://www.hackerrank.com/challenges/diagonal-difference

import           Control.Monad

getList :: Read a => IO [a]
getList = map read . words <$> getLine

main :: IO ()
main = do
  n <- read <$> getLine
  list <- replicateM n getList
  let
    pr = sum $ zipWith (!!) list [0..]
    sn = sum $ zipWith (!!) list [n-1, n-2..]
  print (abs $ pr - sn)
