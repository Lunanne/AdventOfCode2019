module Day7
  (part1, part2, multipleTrusterSignal, runAllMachines) where

import           Control.Lens
import           Data.Function (on)
import           Data.List
import qualified Data.Map      as Map
import           Debug.Trace
import           Intcode       (executeProgram)

part1 :: [Int] -> ([Int], Int)
part1 (register) =
  findPhasesForMaxSignal(register, possiblePhases, machines, input)
  where
    input = [(0, [])]
    machines = ["A", "B", "C", "D", "E"]
    possiblePhases = [0, 1, 2, 3, 4]

findPhasesForMaxSignal::([Int], [Int], [String], [(Int,[Int])]) -> ([Int], Int)
-- findPhasesForMaxSignal (register, possiblePhases, machines, dataStart)| trace ("find phases for max signals " ++ show register ++ " phases to try " ++ show phases ++ " phases output " ++ show phases2) False = undefined
findPhasesForMaxSignal (register, possiblePhases, machines, dataStart) =
  (output, max)
  where
    allData = runAllMachines(register, possiblePhases, machines, dataStart)
    (max, output) = maximumBy (compare `on` fst) allData

runAllMachines::([Int], [Int], [String], [(Int, [Int])]) -> [(Int, [Int])]
-- runAllMachines (register, possiblePhases, machines, dataStart)| trace ("running all machines. machines left " ++ show machines ++ " data so far " ++ show dataStart) False = undefined
runAllMachines(register, possiblePhases, machinesLeft, dataSoFar) =
  output
  where
    newMachinesLeft = tail machinesLeft
    newData = concatMap (\(k,v) -> runNextMachine(register, possiblePhases, k, v)) dataSoFar
    output = if((length newMachinesLeft) /= 0) then runAllMachines(register, possiblePhases, newMachinesLeft, newData) else newData

runNextMachine::([Int], [Int], Int, [Int]) -> [(Int,[Int])]
-- runNextMachine (register, possiblePhases, input, phasesSofar)| trace ("run next Machine input " ++ show input) False = undefined
runNextMachine (register, possiblePhases, input, phasesSoFar ) =
  output
  where
    phasesToTry = possiblePhases \\ phasesSoFar
    output = map (\x -> (singleTrusterSignal(register, x, input), phasesSoFar ++ [x])) (phasesToTry)

multipleTrusterSignal :: ([Int], [Int], Int) -> Int
-- multipleTrusterSignal (register, phases, input)| trace ("multiple truster signal register " ++ show register ++ " phases " ++ show phases ++ " input " ++ show input) False = undefined
multipleTrusterSignal (register, phases, input) =
  result
  where
    result = if((length phases) /= 1) then multipleTrusterSignal(register, (tail phases), single) else single
    single = singleTrusterSignal(register, (head phases), input)

singleTrusterSignal :: ([Int], Int, Int) -> Int
-- singleTrusterSignal (register, phase, input)| trace ("single truster signal register " ++ show register ++ " phase " ++ show phase ++ " input " ++ show input) False = undefined
singleTrusterSignal (register, phase, input) =
  (head result)
  where
    result = executeProgram(register, 0, [phase, input], [])


part2 :: [Int] -> [Int]
part2 (register) =
  executeProgram(register, 0, input, output)
  where
    input = [5]
    output = []
