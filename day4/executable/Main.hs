module Main (main) where

import Text.Printf
import Data.List.Split
import Day4(part1)

main :: IO ()
main = do
    let a = 137683
    let b = 596253
    let result1 = part1(a, b)
    printf "Day 4 Answer part 1 %d \n" result1
    