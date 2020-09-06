module Main (main) where

import Day6 (part1, createTree, part2)
import Text.Printf

main :: IO ()
main = do
    content <- readFile "day6/day6.txt"
    let lins = lines content
    let tree = part1(lins)
    let answer2 = part2(lins)
    print "Day 6"
    print (show tree)
    print (show answer2)

    