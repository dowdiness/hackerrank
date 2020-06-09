module Kangaroo where

-- Kangaroo
-- https://www.hackerrank.com/challenges/kangaroo/problem

solve :: [Int] -> String
solve [x1,v1,x2,v2]
  | v2 < v1 && (x2 - x1) `mod` (v1 - v2) == 0 = "Yes"
  | otherwise  = "No"


main :: IO()
main = interact $ show . solve . map read . words
