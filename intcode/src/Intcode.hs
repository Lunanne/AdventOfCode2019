module Intcode
  ( operations) where

import Control.Lens
import Debug.Trace

add :: (Int, Int, Int, [Int]) -> [Int]
-- add (a, b, c, register)| trace ("add in a " ++ show a ++ ":" ++ show(register !! a)++ " in B " ++ show b ++ ":" ++ show(register !! b) ++ " out " ++ show c) False = undefined

add (inA, inB, out, register) = 
    (element out .~ (a + b)) register 
    where 
        a = register !! inA
        b = register !! inB

multi :: (Int, Int, Int, [Int]) -> [Int]
-- multi (a, b, c, register)| trace ("multi in a " ++ show a  ++ ":" ++ show(register !! a)++ " in B " ++ show b  ++ ":" ++ show(register !! b)++ " out " ++ show c) False = undefined
multi (inA, inB, out, register) = 
    (element out .~ (a * b)) register 
    where 
        a = register !! inA
        b = register !! inB

operations :: (Int) -> (Int, Int, Int, [Int]) -> [Int]
operations (opcode) 
    | opcode == 1 = add
    | otherwise = multi

executeProgram :: ([Int], Int) -> [Int]
-- executeProgram (a, b)| trace ("register " ++ show a ++ " start pos " ++ show b) False = undefined
executeProgram (input, startPos)
  | input !! startPos == 99 = input
  | otherwise = executeProgram(newRegister, startPos + 4)
  where
    newRegister = operations (input !! startPos) (input !! (startPos +1), input !! (startPos +2), input !! (startPos +3), input)
