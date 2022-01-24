module Algorithms.Implementation.DrawingBook where

-- Drawing Book
-- https://www.hackerrank.com/challenges/drawing-book

solve :: [Int] -> Int
solve [x,y] = min fromFront fromLast
    where
        fromFront = y `div` 2
        fromLast = x `div` 2 - fromFront

main :: IO()
main = interact $ show . solve . map read . words
