module Day5
  (part1) where

import Debug.Trace
import Control.Lens
import Intcode(executeProgram)

part1 :: [Int] -> [Int]
part1 (register) = 
  executeProgram(register, 0, input, output)
  where
    input = [1]
    output = []
