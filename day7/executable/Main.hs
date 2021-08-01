module Main (main) where

import Text.Printf
import Data.List.Split
import Day7(part1, part2)

main :: IO ()
main = do
    content <- readFile "day7/day7.txt"
    let arr = splitOn "," content
    let input = map read arr
    let p1 = part1 input
    print (show p1)
    printf "Day 7 Answer part 1 %d \n" (snd p1)