module Main (main) where

import Text.Printf
import Data.List.Split
import Data.List.Sequences
import Day4(part1, part2, thereShouldOnlyBeAPair)
import Data.List
import Data.Maybe

main :: IO ()
main = do
    let a = 137683
    let b = 596253
    let result1 = part1(a, b)
    let result2 = part2(a, b)
    printf "Day 4 Answer part 1 %d \n" result1
    printf "Day 4 Answer part 2 %d \n" result2
    