module Day5
  (part1, part2) where

import Debug.Trace
import Control.Lens
import Intcode(executeProgram)

part1 :: [Int] -> [Int]
part1 (register) = 
  executeProgram(register, 0, input, output)
  where
    input = [1]
    output = []

part2 :: [Int] -> [Int]
part2 (register) = 
  executeProgram(register, 0, input, output)
  where
    input = [5]
    output = []
