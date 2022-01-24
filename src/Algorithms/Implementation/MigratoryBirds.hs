module Algorithms.Implementation.MigratoryBirds where

-- Migratory Birds
-- https://www.hackerrank.com/challenges/migratory-birds

import           Data.Function
import           Data.List

main :: IO()
main = interact $
    show .
    head .
    minimumBy (flip compare `on` length) .
    group .
    sort .
    map (read::String -> Int) .
    tail .
    words
