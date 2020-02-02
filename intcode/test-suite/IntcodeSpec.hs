module IntcodeSpec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Intcode(executeProgram)

spec :: Spec
spec = do
    describe "Intcode operations" $ do
        it "should execute programs correctly" $ do
            executeProgram([1,0,0,0,99], 0) `shouldBe` [2,0,0,0,99]
            executeProgram([2,3,0,3,99], 0) `shouldBe` [2,3,0,6,99]
            executeProgram([2,4,4,5,99,0], 0) `shouldBe` [2,4,4,5,99,9801]
            executeProgram([1,1,1,4,99,5,6,0,99], 0) `shouldBe` [30,1,1,4,2,5,6,0,99]

