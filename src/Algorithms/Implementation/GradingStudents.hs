module Algorithms.Implementation.GradingStudents where

-- Grading Students
-- https://www.hackerrank.com/challenges/grading/problem
roundUp :: Int -> Int
roundUp x
    | x >= 38 && 3 <= m5 = x + (5 - m5)
    | otherwise          = x
    where m5 = x `mod` 5

main :: IO()
main = interact $ unlines . map (show . roundUp . read) . tail . words
