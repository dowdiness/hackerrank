module Algorithms.Implementation.DayoftheProgrammer where

-- Day of the Programmer
-- https://www.hackerrank.com/challenges/day-of-the-programmer

solve :: String -> String
solve y
    | read y < 1918 = if read y `mod` 4 == 0 then "12.09." <> y else "13.09." <> y
    | read y > 1918 = if read y `mod` 400 == 0 || read y `mod` 4 == 0 && read y `mod` 100 /= 0 then "12.09." <> y else "13.09." <> y
    | read y == 1918 = "26.09." <> y

main :: IO()
main = interact solve
