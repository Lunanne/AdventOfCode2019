module Day3
  (part1, part2, parseWormEntry, parseWorm) where

import Debug.Trace
import Util(manhattenDistance, Coordinate(..))
import Data.List
import Data.Maybe

part1 :: ([String],[String]) -> Int
part1 (worm1Input, worm2Input) = 
  head (sort distances)
  where
    start = TwoDCoordinate 0 0
    worm1 = parseWorm(worm1Input, [start])
    worm2 = parseWorm(worm2Input, [start])
    crossPoints = same worm1 worm2
    distances = map calculateManhattenDistance (delete start crossPoints)

part2 :: ([String],[String]) -> Int
part2 (worm1Input, worm2Input) = 
  head (sort steps)
  where
    start = TwoDCoordinate 0 0
    worm1 = parseWorm(worm1Input, [start])
    worm2 = parseWorm(worm2Input, [start])
    crossPoints = same worm1 worm2
    realCP =delete start crossPoints
    steps = calculateSteps (realCP,worm1,worm2,[])


  
parseWorm :: ([String], [Coordinate]) -> [Coordinate]
parseWorm(wormInput, coordinatesSoFar)
  |null wormInput = coordinatesSoFar
  |otherwise = parseWorm(tail wormInput, coordinatesNew)
  where 
    coordinatesNew = coordinatesSoFar ++ parseWormEntry (head wormInput, last coordinatesSoFar)

parseWormEntry:: (String,Coordinate)  -> [Coordinate]
parseWormEntry(entry, TwoDCoordinate startx starty) 
  | entry !! 0 == 'R' = [TwoDCoordinate x y | x <- [startx+1..(startx +value)],y <- [starty]]
  | entry !! 0 == 'L' = [TwoDCoordinate x y | x <- [startx-1, startx-2..(startx-value)],y <- [starty]]
  | entry !! 0 == 'U' = [TwoDCoordinate x y | x <- [startx],y <- [starty+1..(starty+value)]]
  | entry !! 0 == 'D' = [TwoDCoordinate x y | x <- [startx],y <- [starty-1, starty-2..(starty-value)]]
  where
    value = (read (tail entry))

same xs ys = [ x | x <- xs , y <- ys, x == y]

calculateManhattenDistance:: (Coordinate) -> Int
calculateManhattenDistance(to) = let start = TwoDCoordinate 0 0 in manhattenDistance(start, to)

calculateSteps:: ([Coordinate], [Coordinate], [Coordinate], [Int]) -> [Int]
calculateSteps(points, worm1, worm2, soFar)
  |null points = soFar
  |otherwise = calculateSteps(res, worm1, worm2, newVal)
  where 
    pos = head points
    res = tail points
    worm1Steps = fromMaybe(0) $ elemIndex pos worm1
    worm2Steps = fromMaybe(0) $ elemIndex pos worm2
    newVal = soFar ++ [worm1Steps + worm2Steps]
