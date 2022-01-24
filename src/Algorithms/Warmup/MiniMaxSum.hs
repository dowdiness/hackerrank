module Algorithms.Warmup.MiniMaxSum where

-- Mini-Max Sum
-- https://www.hackerrank.com/challenges/mini-max-sum

import           Control.Monad
-- import           Data.Array
import           Data.Bits
import           Data.List
-- import           Data.List.Split
-- import           Data.Set
import           Debug.Trace
import           System.Environment
import           System.IO
import           System.IO.Unsafe

-- Complete the miniMaxSum function below.
miniMaxSum arr = [minSum, maxSum]
  where
    min' = minimum arr
    max' = maximum arr
    maxSum = sum $ Data.List.delete min' arr
    minSum = sum $ Data.List.delete max' arr

main :: IO()
main = do
    arr <- Data.List.map (read :: String -> Int) . words <$> getLine

    putStrLn $ unwords $ Data.List.map show $ miniMaxSum arr
