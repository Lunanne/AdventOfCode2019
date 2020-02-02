module Day3Spec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Day3 (part1, part2, parseWormEntry, parseWorm)
import Util(Coordinate(..))

spec :: Spec
spec = do
    describe "Day 3 Part 1" $ do
        it "should calculate where the wires meet closest to the starting point" $ do
            part1(["R8","U5","L5","D3"],["U7","R6","D4","L4"]) `shouldBe` 6
            part1(["R75","D30","R83","U83","L12","D49","R71","U7","L72"],["U62","R66","U55","R34","D71","R55","D58","R83"]) `shouldBe` 159
            part1(["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]) `shouldBe` 135

        it "should calculate where the wires meet closest to the starting point" $ do
            part2(["R8","U5","L5","D3"],["U7","R6","D4","L4"]) `shouldBe` 30
            part2(["R75","D30","R83","U83","L12","D49","R71","U7","L72"],["U62","R66","U55","R34","D71","R55","D58","R83"]) `shouldBe` 610
            part2(["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]) `shouldBe` 410
    

    
    describe "Day 3 should parse entries correctly" $ do
        it "should parse R correctly" $ do
            parseWormEntry ("R3", TwoDCoordinate 0 0) `shouldBe` [(TwoDCoordinate 1 0),(TwoDCoordinate 2 0),(TwoDCoordinate 3 0)]
            parseWormEntry ("R2", TwoDCoordinate (-1) (-5)) `shouldBe` [(TwoDCoordinate 0 (-5)),(TwoDCoordinate 1 (-5))]

        it "should parse L correctly" $ do
            parseWormEntry ("L3", TwoDCoordinate 0 0) `shouldBe` [(TwoDCoordinate (-1) 0),(TwoDCoordinate (-2) 0),(TwoDCoordinate (-3) 0)]
            parseWormEntry ("L2", TwoDCoordinate (-1) (-5)) `shouldBe` [(TwoDCoordinate (-2) (-5)),(TwoDCoordinate (-3) (-5))]

        it "should parse U correctly" $ do
            parseWormEntry ("U3", TwoDCoordinate 0 0) `shouldBe` [(TwoDCoordinate 0 1),(TwoDCoordinate 0 2),(TwoDCoordinate 0 3)]
            parseWormEntry ("U2", TwoDCoordinate (-1) (-5)) `shouldBe` [(TwoDCoordinate (-1) (-4)),(TwoDCoordinate (-1) (-3))]

        it "should parse D correctly" $ do
            parseWormEntry ("D3", TwoDCoordinate 0 0) `shouldBe` [(TwoDCoordinate 0 (-1)),(TwoDCoordinate 0 (-2)),(TwoDCoordinate 0 (-3))]
            parseWormEntry ("D2", TwoDCoordinate (-1) (-5)) `shouldBe` [(TwoDCoordinate (-1) (-6)),(TwoDCoordinate (-1) (-7))]

        it "should parse worms correctly" $ do
            parseWorm (["R8","U5","L5","D3"], [TwoDCoordinate 0 0]) `shouldBe` [TwoDCoordinate 0 0, TwoDCoordinate 1 0,TwoDCoordinate 2 0,TwoDCoordinate 3 0,TwoDCoordinate 4 0,TwoDCoordinate 5 0,TwoDCoordinate 6 0,TwoDCoordinate 7 0,TwoDCoordinate 8 0,TwoDCoordinate 8 1,TwoDCoordinate 8 2,TwoDCoordinate 8 3,TwoDCoordinate 8 4,TwoDCoordinate 8 5,TwoDCoordinate 7 5,TwoDCoordinate 6 5,TwoDCoordinate 5 5,TwoDCoordinate 4 5,TwoDCoordinate 3 5,TwoDCoordinate 3 4,TwoDCoordinate 3 3,TwoDCoordinate 3 2]
            parseWorm (["U7","R6","D4","L4"], [TwoDCoordinate 0 0]) `shouldBe` [TwoDCoordinate 0 0,TwoDCoordinate 0 1,TwoDCoordinate 0 2,TwoDCoordinate 0 3,TwoDCoordinate 0 4,TwoDCoordinate 0 5,TwoDCoordinate 0 6,TwoDCoordinate 0 7,TwoDCoordinate 1 7,TwoDCoordinate 2 7,TwoDCoordinate 3 7,TwoDCoordinate 4 7,TwoDCoordinate 5 7,TwoDCoordinate 6 7,TwoDCoordinate 6 6,TwoDCoordinate 6 5,TwoDCoordinate 6 4,TwoDCoordinate 6 3,TwoDCoordinate 5 3,TwoDCoordinate 4 3,TwoDCoordinate 3 3,TwoDCoordinate 2 3]
