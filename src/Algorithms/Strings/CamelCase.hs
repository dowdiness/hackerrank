module Algorithms.Strings.CamelCase where

-- CamelCase
-- https://www.hackerrank.com/challenges/camelcase

import           Data.Char

camelcase :: String -> String
camelcase =
    show .
    (+ 1) .
    length .
    filter isUpper

main :: IO()
main = interact camelcase
