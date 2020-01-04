module Main (main) where

import Text.Printf
import Data.List.Split
import Day2(part1, part2)

main :: IO ()
main = do
    content <- readFile "day2/day2.txt"
    let arr = splitOn "," content
    let input = map read arr
    let p1 = part1 input
    printf "Day 2 Answer part 1 %d \n" p1
    let p2 = part2(input,0)
    printf "Day 2 Answer part 2 %d \n" p2
