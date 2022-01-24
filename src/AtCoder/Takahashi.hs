module AtCoder.Takahashi where

import qualified Data.ByteString.Char8 as BS
import           Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

isInt :: Real a => a -> Bool
isInt x = x == fromInteger (round (realToFrac x))

times :: Real a => a -> Int
times x = (+1) . length $ takeWhile (not . isInt) $ map (\y -> 360*y / realToFrac x) [1..]

solve :: Int -> Integer -> Int
solve n x = 360 * n `div` fromIntegral x

main = do
  x <- getInt
  print $ solve (times x) x
