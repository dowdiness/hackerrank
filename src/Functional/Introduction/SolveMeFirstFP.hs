module Functional.Introduction.SolveMeFirstFP where

-- Solve Me First FP
-- https://www.hackerrank.com/challenges/fp-solve-me-first

main = interact $ show . sum . map read . words
