module Day7Spec
  ( spec
  ) where

import           Data.Maybe
import           Day7                  (multipleTrusterSignal, part1)
import           Test.Hspec
import           Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Day 7" $ do
    it "should calculate truster signal correctly " $ do
      multipleTrusterSignal( [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0], [4, 3, 2, 1, 0], 0) `shouldBe` 43210
      multipleTrusterSignal( [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0], [0,1,2,3,4], 0) `shouldBe` 54321
      multipleTrusterSignal( [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0], [1,0,4,3,2], 0) `shouldBe` 65210

    it "should calculate max truster signal correctly " $ do
      part1( [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]) `shouldBe` ([4, 3, 2, 1, 0], 43210)
      part1( [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) `shouldBe` ([0,1,2,3,4], 54321)
      part1( [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) `shouldBe` ([1,0,4,3,2], 65210)
