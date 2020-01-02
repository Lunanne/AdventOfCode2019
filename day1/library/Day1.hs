module Day1
  ( calculateMassFuel
  , calculateTotalFuel
  ) where

import Debug.Trace
calculateFuel :: (Int) -> Int
calculateFuel (m) = (m `div` 3) - 2


calculateMassesFuel :: ([Int]) -> [Int]
calculateMassesFuel (masses) = map calculateFuel masses

calculateMassFuel :: ([Int]) -> Int
calculateMassFuel (masses) = sum (calculateMassesFuel masses)

calculateFuelFuel :: (Int) -> Int
calculateFuelFuel (f)| trace (show f) False = undefined
calculateFuelFuel (f) =
  let fuelCost = calculateFuel f
   in if fuelCost <= 0
        then 0
        else fuelCost + calculateFuelFuel fuelCost

-- 4928963
-- 4931807 //too high
calculateTotalFuel :: ([Int]) -> Int
calculateTotalFuel (masses) =
  let fuels = map calculateFuelFuel (calculateMassesFuel masses)
  in sum(fuels) + calculateMassFuel(masses)
