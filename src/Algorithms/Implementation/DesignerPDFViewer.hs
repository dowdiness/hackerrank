module Algorithms.Implementation.DesignerPDFViewer where

-- Designer PDFV iewer
-- https://www.hackerrank.com/challenges/designer-pdf-viewer

import           Data.Maybe

solve :: [String] -> Int
solve xs = length word * maximum wordHeight
  where
    height  = map (read :: String -> Int) $ take 26 xs
    word    = last xs
    wordHeight = map (height!!) $ fromJust <$> map (`lookup` alphabets) word

alphabets :: [(Char, Int)]
alphabets = zip ['a'..'z'] [0..]

main :: IO()
main = interact $ show . solve . words
