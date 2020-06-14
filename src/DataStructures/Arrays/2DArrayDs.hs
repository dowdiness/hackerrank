module DataStructures.Arrays.TwoDArrayDs where

-- 2D Array - DS
-- https://www.hackerrank.com/challenges/2d-array

glassheads = (+) . (*6) <$> [0..3] <*> [0..3]

solve :: [Int] -> Int
solve arr = maximum $ calc . (`drop` arr) <$> glassheads

calc :: [Int] -> Int
calc ar = sum $ fln ++ [sln] ++ tln
    where
        fln = take 3 ar
        sln = ar !! 7
        tln = take 3 $ drop 12 ar

main :: IO()
main = interact $ show . solve . map (read :: String -> Int) . words
