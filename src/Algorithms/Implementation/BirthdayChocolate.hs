module Algorithms.Implementation.BirthdayChocolate where

-- Birthday Chocolate
-- https://www.hackerrank.com/challenges/the-birthday-bar

takeN :: Int -> [a] -> [[a]]
takeN _ []     = []
takeN n (x:xs)
  | length xs >= n - 1 = take n (x:xs) : takeN n xs
  | otherwise      = []

getList :: IO [Int]
getList = map (read:: String -> Int) . words <$> getLine

main :: IO()
main = do
    getLine
    cakes <- getList
    [bd, bm] <- getList
    print $ length $ filter (== bd) $ map sum $ takeN bm cakes
