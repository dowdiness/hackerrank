{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}

module AtCoder.Abc236.C where

import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe

readInt = fst . fromJust . BS.readInteger
readIntList = map readInt . BS.words

readLineInputs = BS.words <$> BS.getLine

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine

solve :: [BS.ByteString] -> [BS.ByteString] -> [Bool]
solve (a:as) bs@(b:bs') | a == b = True : solve as bs'
solve (_ : as) bs       = False : solve as bs
solve _ _               = []

main :: IO ()
main = do
  [n,m] <- getIntList
  as <- readLineInputs
  bt <- readLineInputs
  putStrLn $ unlines $ map (\a -> if a then "Yes" else "No") $ solve as bt
