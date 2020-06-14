module Algorithms.Implementation.ElectronicsShop
    where

-- Electronics Shop
-- https://www.hackerrank.com/challenges/electronics-shop

import           Control.Monad
import           Data.List
import           Data.Maybe

getList :: IO [Int]
getList = map (read:: String -> Int) . words <$> getLine

solve :: Int -> [Int] -> [Int] -> Int
solve b k d =
    fromMaybe (-1) $
    listToMaybe $
    sortBy (flip compare) $
    filter (<=b) $
    (+) <$> k <*> d

main :: IO ()
main = do
    [n,_,_] <- getList
    keyboards <- getList
    drives <- getList
    print $ solve n keyboards drives
    return ()
