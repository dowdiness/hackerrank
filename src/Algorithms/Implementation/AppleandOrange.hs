module Algorithms.Implementation.AppleandOrange where

-- Apple and Orange
-- https://www.hackerrank.com/challenges/apple-and-orange/problem

import           Control.Monad

solve s t a b apples oranges = unlines [an, on]
    where
        an = show $ length $ filter (\n -> n >= s && n <= t) $ map (+a) apples
        on = show $ length $ filter (\n -> n >= s && n <= t) $ map (+b) oranges

getList :: Read a => IO [a]
getList = map read . words <$> getLine

main :: IO()
main = do
    [[s,t], [a,b], [m,n], apples, oranges] <- replicateM 5 getList
    putStrLn $ solve s t a b apples oranges
    return ()
