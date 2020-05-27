module Intcode
  ( operations, executeProgram, add, multi, setRegister, getOperationInput) where

import Control.Lens
import Debug.Trace
import Data.NumberLength
import Data.Digits
import Data.Maybe

add :: ([Int], [Int], [Int], [Int], [Int]) -> ([Int], Maybe [Int])
add (modes, input, register, inStream, outStream)| trace ("add modes " ++ show modes ++ "input" ++ show input) False = undefined

add (modes, input, register,inStream, outStream) = 
    ((element out .~ (inA + inB)) register, Nothing)
    where
      out = register !! (input !! 2)
      modeA = modes !! 0
      modeB = modes !! 1
      inA = if(modeA == 0) then register !! (input !! 0) else input !! 0
      inB = if(modeB == 0) then register !! (input !! 1) else input !! 1

multi :: ([Int], [Int], [Int], [Int], [Int]) -> ([Int], Maybe [Int])
multi (modes, input, register,inStream, outStream)| trace ("multi modes " ++ show modes ++ " input " ++ show input) False = undefined
multi (modes, input, register,inStream, outStream) = 
    ((element out .~ (inA * inB)) register, Nothing)
    where
      out = register !! (input !! 2)
      modeA = modes !! 0
      modeB = modes !! 1
      inA = if(modeA == 0) then register !! (input !! 0) else input !! 0
      inB = if(modeB == 0) then register !! (input !! 1) else input !! 1

setRegister :: ([Int], [Int], [Int], [Int], [Int]) -> ([Int], Maybe [Int])
setRegister (modes, input, register, inStream, outStream)| trace ("set  input " ++ show input) False = undefined
setRegister(modes, input, register, inStream, outStream) =
      ((element parameter .~ inA) register, Just newStream)
       where
        inA = head inStream
        parameter = register !! (input !! 0)
        newStream = tail inStream

readRegister :: ([Int], [Int], [Int], [Int], [Int]) -> ([Int], Maybe [Int])
readRegister (modes, input, register, inStream, outStream)| trace ("read output" ++ show (register !! (input !! 0))) False = undefined
readRegister(modes, input, register, inStream, outStream) = 
  (register, Just newStream)
  where
    modeA = modes !! 0
    outA = if(modeA == 0) then register !! (input !! 0) else input !! 0
    newStream = outStream ++ [outA]
    

operations :: (Int) -> (([Int], [Int], [Int], [Int], [Int]) -> ([Int], Maybe [Int]), Int)
operations (opcode) 
    | opcode == 1 = (add, 3)
    | opcode == 2 = (multi, 3)
    | opcode == 3 = (setRegister, 1)
    | opcode == 4 = (readRegister, 1)
    | otherwise = (readRegister, 1)

executeProgram :: ([Int], Int, [Int], [Int]) -> [Int]
-- executeProgram (a, b)| trace ("register " ++ show a ++ " start pos " ++ show b) False = undefined
executeProgram (input, startPos, inStream, outStream)
  | (input !! startPos) == 99 = if((length outStream) /=0) then outStream else input
  | otherwise = executeProgram(newRegister, startPos + operationLength + 1, newInStream, newOutStream)
  where
    operationInput = getOperationInput(startPos, input)
    opCode = getFst operationInput
    operationDetails = operations (opCode) 
    operation = fst operationDetails
    operationLength = snd operationDetails
    operationOutput = operation(getSnd operationInput, getThrd operationInput, input, inStream, outStream)
    newRegister = fst operationOutput
    newInStream = if(opCode == 3) then fromJust (snd operationOutput) else inStream
    newOutStream = if(opCode == 4) then fromJust (snd operationOutput) else outStream
    

getOperationInput :: (Int, [Int]) -> (Int, [Int], [Int])
getOperationInput(startPos, input) = 
  (opcode, modes, inputOperation)
  where
    instruction = input !! startPos
    opcode = unDigits 10 (Prelude.drop (numberLength(instruction) -2 ) (digits 10 instruction))
    operationDetails = operations (opcode) 
    operationLength = snd operationDetails
    modes = Prelude.take operationLength $ reverse(Prelude.take(numberLength(instruction) -2) (digits 10 instruction)) ++ repeat 0
    inputOperation = take operationLength $ drop (startPos+1) $ input

getFst (x, _, _) = x
getSnd (_, x, _) = x
getThrd (_, _, x) = x