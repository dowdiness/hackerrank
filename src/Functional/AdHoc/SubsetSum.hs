{-# LANGUAGE ScopedTypeVariables #-}

module Functional.AdHoc.SubsetSum where

-- Subset Sum
-- https://www.hackerrank.com/challenges/subset-sum

-- Unsolved take toomuch time

import           Control.Monad
import qualified Data.Vector.Unboxed as VU

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

getList :: Read a => IO [a]
getList = map read . words <$> getLine

max' :: Int -> [Int] -> Int
max' y ys = maximum $ map sum $ comb y ys

maxList :: [Int] -> [Int]
maxList ys = take (length ys) $ map (`max'` ys) [1..]

compareList :: Int -> [Int] -> Int
compareList y ys = (\x -> if x >= length ys then -1 else x + 1 ) $ length $ filter (<y) ys

solve :: [Int] -> [Int]
solve xs = xs

main :: IO ()
main = do
    [_ :: Int]   <- getList
    xs :: [Int]  <- getList
    [n]          <- getList
    ys :: [Int]  <- map read <$> replicateM n getLine
    putStrLn $ unlines $ map (show . (\y -> compareList y $ maxList xs)) ys
    return ()
