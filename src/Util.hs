module Util where

getList :: Read a => IO [a]
getList = map read . words <$> getLine
