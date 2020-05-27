module Day5
  (part1) where

import Debug.Trace
import Control.Lens
import Intcode(executeProgram)

part1 :: [Int] -> Int
part1 (register) = 
  last (executeProgram(correctedRegister, 0, input, output))
  where
    tmp = (element 1 .~ 12) register
    correctedRegister = (element 2 .~ 2) tmp
    input = [1]
    output = []