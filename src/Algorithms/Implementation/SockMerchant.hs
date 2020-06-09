module SockMerchant where

-- SockMerchant
-- https://www.hackerrank.com/challenges/sock-merchant

import           Data.List

countPairs :: (Int, String) -> (Int, String) -> (Int, String)
countPairs (count, acc) (_, n) = if acc == n then (count + 1, "drop") else (count, n)

-- Complete the sockMerchant function below.
sockMerchant :: [String] -> Int
sockMerchant [] = 0
sockMerchant xs =
    fst $ foldl1' countPairs $ map (0, y) xs

main :: IO()
main = interact $ show . sockMerchant . sort . tail . words
