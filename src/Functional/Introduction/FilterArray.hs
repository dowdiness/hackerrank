module Functional.Introduction.FilterArray where

-- Filter Array
-- https://www.hackerrank.com/challenges/fp-filter-array

f :: Int -> [Int] -> [Int]
f n = filter (< n)

-- The Input/Output section. You do not need to change or modify this part
main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    let
        numbers = map read (lines inputdata) :: [Int]
    putStrLn . unlines $ (map show . f n) numbers
