module Day4
  (part1, isNumberAscending, isThereAPair, thereShouldOnlyBeAPair) where

import Debug.Trace
import Data.List
import Control.Lens
import Data.Digits
import Text.Printf

part1 :: (Int, Int) -> Int
part1(from, to) = 
  length result
  where 
    t = [digits 10 x  | x <- [from..to]]
    temp = filter isNumberAscending t
    result = filter isThereAPair temp

isNumberAscending::[Int] -> Bool
isNumberAscending(number) = number == sort number

isThereAPair::[Int] -> Bool
isThereAPair(number) = elem True (zipWith (==) (number) (tail number) )

thereShouldOnlyBeAPair::[Int] -> Bool
thereShouldOnlyBeAPair(number) = 
  notElem True (zipWith (\a b -> !(a==True && b==True) (t) (tail t) )
  where 
    t = zipWith (==) (number) (tail number) 
