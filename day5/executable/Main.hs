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
    let diagnostics = all (== 0) (init p1) 
    print (show p1)
    printf "Day 5 diagnostics success %s\n" (show diagnostics)
    printf "Day 5 Answer part 1 %d \n" (last p1)

