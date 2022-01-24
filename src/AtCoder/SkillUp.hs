module AtCoder.SkillUp where

import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

getIntList = readIntList <$> BS.getLine
getIntMatrix = map readIntList . BS.lines <$> BS.getContents

toTable :: [[Integer]] -> [(Integer,[Integer])]
toTable = map (\book -> (head book, tail book))

solve :: Integer -> [(Integer,[Integer])] -> Integer
solve target table = if null allcosts then -1 else minimum allcosts
  where
    allcases = filter (not . null) $ subsequences table
    allcosts = map fst . filter (\(_, xs) -> all (>= target) xs) . map calc $ allcases

calc :: [(Integer,[Integer])] -> (Integer,[Integer])
calc []          = (0, repeat 0)
calc ((t, c):cs) = (t + tt, zipWith (+) c cc)
  where
    (tt, cc) = calc cs

main = do
  [_, _, target] <- getIntList
  contents <- getIntMatrix
  print $ solve target $ toTable contents

lis :: [Int] -> [Int]
lis []       = []
lis (x:[y])  = if x < y then x:[y] else []
lis (x:y:xs) = if x < y then x:lis (y:xs) else lis (y:xs)
