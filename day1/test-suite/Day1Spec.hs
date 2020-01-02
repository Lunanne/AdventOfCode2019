module Day1Spec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Day1 (calculateMassFuel, calculateTotalFuel)

spec :: Spec
spec = do
    describe "Day 1 Part 1" $ do
        it "should calculate the fuel needed for mass" $ do
            calculateMassFuel [12] `shouldBe` 2
            calculateMassFuel [14] `shouldBe` 2
            calculateMassFuel [1969] `shouldBe` 654
            calculateMassFuel [100756] `shouldBe` 33583
    
    describe "Day 1 Part 2" $ do
        it "should calculate the fuel needed for mass and fuel" $ do
            calculateTotalFuel [14] `shouldBe` 2
            calculateTotalFuel [1969] `shouldBe` 966
            calculateTotalFuel [100756] `shouldBe` 50346