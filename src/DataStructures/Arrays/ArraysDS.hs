module DataStructures.Arrays.ArraysDS where

-- Arrays - DS
-- https://www.hackerrank.com/challenges/arrays-ds/problem

main :: IO()
main = interact $ unwords . reverse . tail . words
