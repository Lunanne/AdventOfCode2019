module Main (main) where

import Text.Printf
import Data.List.Split
import Day5(part1)

main :: IO ()
main = do
    content <- readFile "day5/day5.txt"
    let arr = splitOn "," content
    let input = map read arr
    let p1 = part1 input
    printf "Day 5 Answer part 1 %d \n" p1
