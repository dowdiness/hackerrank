module Util (comb, getList, isPrime, mkPair, breakPr, tsun) where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.ByteString.Char8       as BS
import           Data.List
import           Data.Maybe
import qualified Data.Set                    as S
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as UM

getList :: Read a => IO [a]
getList = map read . words <$> getLine

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | null [x | x <- [2 .. n-1], n `mod` x == 0] = True
    | otherwise = False

mkPair :: [a] -> [(a,a)]
mkPair []       = []
mkPair [x]      = [(x,x)]
mkPair (x:y:xs) = (x,y) : mkPair xs

solve :: String -> String
solve xs
  | res == xs                   = xs
  | not $ null res && res /= xs = solve res
  | null res                    = "Empty String"
  where
    res = breakPr xs

breakPr :: Eq a => [a] -> [a]
breakPr []       = []
breakPr [x]      = [x]
breakPr (x:y:xs) = if x == y then xs else x : breakPr (y : xs)

comb :: Int -> [a] -> [[a]]
comb 0 _      = [[]]
comb _ []     = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

main :: IO()
main = interact solve

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

cut :: String -> Int
cut input = maximum $ map length leng
  where
    leng  = map (uncurry intersect . mapTuple nub) part
    part = map (`splitAt` input) [1..len]
    len  = length input


tuplify2 (x:y:_) = (x,y)
tuplify2 _       = undefined

--Input functions with ByteString
readInt = fst . fromJust . BS.readInteger
readIntTuple = tuplify2 . map readInt . BS.words
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine
getIntMatrix = map readIntList . BS.lines <$> BS.getContents
getIntTuple = readIntTuple <$> BS.getLine
getIntNTuples n = map readIntTuple <$> replicateM (fromIntegral n) BS.getLine
getIntTuples = map readIntTuple . BS.lines <$> BS.getContents


tsun :: Integer -> [Integer] -> [Integer] -> [Integer] -> Int
tsun k s [] [] = length s
tsun k s [x] [] = if sum s + x <= k then length s + 1 else length s
tsun k s [] [y] = if sum s + y <= k then length s + 1 else length s
tsun k s (x:xs) [] = if sum s + x <= k then tsun k (x:s) [] xs else length s
tsun k s [] (y:ys) = if sum s + y <= k then tsun k (y:s) [] ys else length s
tsun k s (x:xs) [y]
  | x <= y && sum s + x <= k = tsun k (x:s) xs [y]
  | x > y && sum s + y <= k = tsun k (y:s) (x:xs) []
  | otherwise  = length s
tsun k s [x] (y:ys)
  | x <= y && sum s + x <= k = tsun k (x:s) [] (y:ys)
  | x > y && sum s + y <= k = tsun k (y:s) [x] ys
  | otherwise  = length s
tsun k s (x:xs) (y:ys)
  | x <= y && sum s + x <= k = tsun k (x:s) xs (y:ys)
  | x > y && sum s + y <= k = tsun k (y:s) (x:xs) ys
  | otherwise  = length s
