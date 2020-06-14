module Algorithms.Implementation.CountingValleys where

-- Counting Valleys
-- https://www.hackerrank.com/challenges/counting-valleys

import           Data.List

step :: Char -> Int
step 'U' = 1
step 'D' = -1

solve :: String -> Int
solve  = length
        . filter (all (<0))
        . groupBy (\x y -> x /= 0 && y /= 0)
        . scanl (+) 0
        . map step

main :: IO()
main = interact $ show . solve . head . tail . words
