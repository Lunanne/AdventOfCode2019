module Day4
  (part1, isNumberAscending, isThereAPair) where

import Debug.Trace
import Data.List
import Control.Lens
import Data.Digits

part1 :: (Int, Int) -> Int
part1(from, to) = 
  length result
  where 
    temp = [x | x <- [from..to] isNumberAscending(digits x)]
    result = [x | x <- temp isThereAPair(digits x)]

isNumberAscending::[Int] -> Bool
isNumberAscending(number) = number == sort number

isThereAPair::[Int] -> Bool
isThereAPair(number) = elem True (zipWith (==) (number) (tail number) )
