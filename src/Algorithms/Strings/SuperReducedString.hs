module Algorithms.Strings.SuperReducedString where

-- Super Reduced String
-- https://www.hackerrank.com/challenges/reduced-string

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

main :: IO()
main = interact solve
