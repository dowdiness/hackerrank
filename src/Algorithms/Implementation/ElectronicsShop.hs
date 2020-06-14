module Algorithms.Implementation.ElectronicsShop
    where

-- Electronics Shop
-- https://www.hackerrank.com/challenges/electronics-shop

getList :: IO [Int]
getList = map (read:: String -> Int) . words <$> getLine

solve :: Int -> [Int] -> [Int] -> Int
solve b k d = maximum $ (\x -> if null x then [-1] else x ) $ filter (<=b) $ (+) <$> k <*> d

main :: IO ()
main = do
    [n,_,_] <- getList
    keyboards <- getList
    drives <- getList
    print $ solve n keyboards drives
    return ()
