module Functional.Introduction.HelloWorldNTimes where

-- Hello World N Times
-- https://www.hackerrank.com/challenges/fp-hello-world-n-times

main :: IO()
main = interact $ unlines . flip replicate "Hello World" . read
