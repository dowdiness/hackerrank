module Functional.MemoizationDP.PentagonalNumbers where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

-- Answer 1
-- Very slow

-- penta :: [Int] -> [Int]
-- penta [] = []
-- penta xs =　map (\x -> sum [y * 3 + 1 | y <- [0..x - 1]]) xs

-- main :: IO ()
-- main = interact $ unlines . map show . penta . map read . tail . words

-- Answer 2
-- Slow

-- memoPenta :: [Int] -> [Int]
-- memoPenta xs = map (\x -> pentaList!!(x - 1)) xs
--   where
--     maxN = maximum xs + 1
--     penta x = (x - 1) * 3 + 1
--     pentaList = tail $ take maxN $ scanl (\x y -> penta y + x) 0 [1..]

-- main :: IO ()
-- main = interact $ unlines . map show . memoPenta . map read . tail . words

-- Answer 3
-- Actuary work

penta :: Int -> Int
penta n = n * (n * 3 - 1) `div` 2

main :: IO ()
main = interact $ unlines . map (show . penta . read) . tail . words


-- readInt = fst . fromJust . BS.readInteger
-- readIntList = map readInt . BS.words

-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith' f (x:xs) (y:ys) =
--   let z = f x y
--   in z `seq` (z : zipWith' f xs ys)

-- memoPenta :: [Int] -> [Int]
-- memoPenta !xs = map (\x -> pentaList !! (x - 1)) xs
--   where
--     maxN = maximum xs + 1
--     penta x = (x - 1) * 3 + 1
--     pentaList = tail $ take maxN $ scanl (\x y -> penta y + x) 0 [1..]

-- main :: IO ()
-- main = interact $ unlines . map show . memoPenta . map read . tail . words
