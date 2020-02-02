module Day4Spec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Day4 (isNumberAscending, isThereAPair)

spec :: Spec
spec = do
    describe "Day 4 filters" $ do
        it "should validate that the number is ascending" $ do
            isNumberAscending [1, 2, 3] `shouldBe` True
            isNumberAscending [1, 1, 1] `shouldBe` True
            isNumberAscending [1, 2, 1] `shouldBe` False
            isNumberAscending [3, 2, 1] `shouldBe` False

        it "should validate that there is a pair" $ do
            isThereAPair [1, 2, 3] `shouldBe` False
            isThereAPair [1, 1, 1] `shouldBe` True
            isThereAPair [1, 2, 2] `shouldBe` True
            isThereAPair [3, 2, 1] `shouldBe` False