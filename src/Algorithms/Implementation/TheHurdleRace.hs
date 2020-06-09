module TheHurdleRace where

import           Control.Monad
-- The Hurdle Race
-- https://www.hackerrank.com/challenges/the-hurdle-race/problem

solve :: [Int] -> Int
solve (x:xs) = if maximum xs <= x then 0 else maximum xs - x

main :: IO ()
main = interact $ show . solve . map (read :: String -> Int) . tail . words
