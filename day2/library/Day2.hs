module Day2
  (part1, part2) where

import Debug.Trace
import Control.Lens
import Intcode(executeProgram)

part1 :: [Int] -> Int
part1 (register) = 
  executeProgram(correctedRegister, 0) !! 0
  where
    tmp = (element 1 .~ 12) register
    correctedRegister = (element 2 .~ 2) tmp

part2 :: ([Int], Int) -> Int
-- part2 (register, noun)| trace ("part2 noun " ++ show noun) False = undefined
part2 (register, noun)
  | raiseVerb(corrected, 0) /=0 = (100* noun) + raiseVerb(corrected, 0)
  | noun > 99 = 0
  | otherwise = part2(register, noun + 1)
  where
    corrected = (element 1 .~ noun) register

raiseVerb :: ([Int], Int) -> Int
-- raiseVerb (register, verb)| trace ("part2 verb " ++ show verb) False = undefined
raiseVerb (register, verb)
  | executeProgram(correctedRegister, 0) !! 0 == 19690720 = verb
  | verb > 99 = 0
  | otherwise = raiseVerb(register,verb + 1)
  where
    correctedRegister = (element 2 .~ verb) register