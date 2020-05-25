module Main (main) where

import Day1 (calculateMassFuel,calculateTotalFuel)
import Text.Printf

main :: IO ()
main = do
    content <- readFile "day1/day1.txt"
    let lins = lines content
    let masses = map (read) lins
    let part1 = calculateMassFuel masses
    printf "Day 1 Answer part 1 %d \n" part1
    let part2 = calculateTotalFuel masses
    printf "Day 1 Answer part 2 %d \n" part2