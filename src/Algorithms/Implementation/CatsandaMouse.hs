module Algorithms.Implementation.CatsandaMouse where

-- Cats and a Mouse
-- https://www.hackerrank.com/challenges/cats-and-a-mouse

solve :: (Int,Int,Int)  -> String
solve (ca, cb, m)
  | abca == abcb = "Mouse C"
  | abca < abcb  = "Cat A"
  | abca > abcb  = "Cat B"
  where
    abca = abs (m - ca)
    abcb = abs (m - cb)

-- Non-exhaustive patterns
mkTriad :: [Int] -> [(Int,Int,Int)]
mkTriad []         = []
mkTriad (x:y:z:xs) = (x,y,z) : mkTriad xs

main :: IO()
main = interact $ unlines . map solve . mkTriad . map read . tail . words
