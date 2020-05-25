module Day4
  (part1, isNumberAscending, isThereAPair, thereShouldOnlyBeAPair, part2) where

import Debug.Trace
import Data.List
import Control.Lens
import Data.Digits
import Text.Printf
import Data.List.Sequences
import Data.Maybe

part1 :: (Int, Int) -> Int
part1(from, to) = 
  length result
  where 
    t = [digits 10 x  | x <- [from..to]]
    temp = filter isNumberAscending t
    result = filter isThereAPair temp

part2 :: (Int, Int) -> Int
part2(from, to) = 
  length result
  where 
    t = [digits 10 x  | x <- [from..to]]
    temp = filter isNumberAscending t
    temp2 = filter isThereAPair temp
    result = filter thereShouldOnlyBeAPair temp2

isNumberAscending::[Int] -> Bool
isNumberAscending(number) = number == sort number

isThereAPair::[Int] -> Bool
isThereAPair(number) = elem True (comparePairs number)

thereShouldOnlyBeAPair::[Int] -> Bool
thereShouldOnlyBeAPair(number) = 
  isJust(find (\a -> length(a)==2) subSequences)
  where 
    subSequences = splitSeq (==) number
    

comparePairs::[Int] -> [Bool]
comparePairs(number) = zipWith (==) (number) (tail number) 