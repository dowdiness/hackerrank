module DesignerPDFViewer where

-- Designer PDFV iewer
-- https://www.hackerrank.com/challenges/designer-pdf-viewer

solve :: [String] -> Int
solve xs = maximum $ height
  where
    height  = map (read :: String -> Int) $ take 26 xs
    word    = last xs

convert c = map () alphabets

alphabets = zip [0..] ['a'..'z']

main :: IO()
main = interact $ show . solve . words
