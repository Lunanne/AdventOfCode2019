module Util
  (manhattenDistance, Coordinate(..)) where

import Debug.Trace

data Coordinate = TwoDCoordinate Int Int | ThreeDCoordinate Int Int Int deriving(Eq, Show)

manhattenDistance ::(Coordinate, Coordinate) -> Int
manhattenDistance (TwoDCoordinate xa ya, TwoDCoordinate xb yb) = abs(xa-xb) + abs(ya-yb)