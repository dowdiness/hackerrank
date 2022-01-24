module AtCoder.ForbiddenList where

import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

solve :: Integer -> [Integer] -> Integer
solve n [] = n
solve n xs = head [n + b * a | a <- [0..n], b <- [-1, 1], (n + b * a) `notElem` xs]

main :: IO ()
main = do
  [n,m] <- getIntList
  fl <- if m == 0 then return [] else getIntList
  print $ solve n fl
