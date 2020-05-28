module IntcodeSpec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Intcode(executeProgram, add, multi, setRegister, jumpTrue, jumpFalse, lessThan, equals)
import Data.Maybe

spec :: Spec
spec = do
    describe "Intcode operations" $ do
        it "should add the numbers" $ do
            fst (add([1,1,1], [1,1,1], [1,0,0], [1],[1])) `shouldBe` [1,2,0]
            fst (add([0,0,0], [1,1,1], [1,0,0], [1],[1])) `shouldBe` [1,0,0]

        it "should multiply the numbers" $ do
            fst (multi([1,1,1], [2,3,1], [1,1,1], [1], [1])) `shouldBe` [1,6,1]
            fst (multi([0,0,0], [2,3,1], [1,1,1, 1], [1], [1])) `shouldBe` [1,1,1,1]

        
        it "should set the register" $ do
            fst (setRegister([0,0], [0], [1,1,1],[-1], [1])) `shouldBe` [-1, 1, 1]
        
        
        it "should jump if true" $ do
            fromJust (snd (jumpTrue([1,1],[1,23],[0,0,0],[],[]))) `shouldBe` [23]
            snd (jumpTrue([0,1],[1,23],[0,0,0],[],[])) `shouldBe` Nothing
        
        it "should jump if false" $ do
            fromJust (snd (jumpFalse([0,1],[1,23],[0,0,0],[],[]))) `shouldBe` [23]
            snd (jumpFalse([1,1],[1,2],[0,0,0],[],[])) `shouldBe` Nothing
        
        it("should less than correctly") $ do
            fst (lessThan([1, 1], [1, 2, 2], [0, 0, 0], [], [])) `shouldBe` [0,0,1]
            fst (lessThan([0,0], [1, 2, 2], [0,0,0], [],[])) `shouldBe` [0,0,0]
        
        it "should equals correctly" $ do
            fst(equals([1, 1], [1, 2, 2], [0, 0, 0], [], [])) `shouldBe` [0,0,0]
            fst(equals([0,0], [1, 2, 2], [0,0,0], [],[])) `shouldBe` [0,0,1]

        it "should execute programs correctly" $ do
            executeProgram([1,0,0,0,99], 0, [], []) `shouldBe` [2,0,0,0,99]
            executeProgram([2,3,0,3,99], 0, [], []) `shouldBe` [2,3,0,6,99]
            executeProgram([2,4,4,5,99,0], 0, [], []) `shouldBe` [2,4,4,5,99,9801]
            executeProgram([1,1,1,4,99,5,6,0,99], 0, [], []) `shouldBe` [30,1,1,4,2,5,6,0,99]
            executeProgram([1101,100,-1,4,0], 0, [], []) `shouldBe` [1101,100,-1,4,99]


        it "should compare correctly to 8" $ do
            executeProgram([3,9,8,9,10,9,4,9,99,-1,8], 0, [8], []) `shouldBe` [1]
            executeProgram([3,9,8,9,10,9,4,9,99,-1,8], 0, [9], []) `shouldBe` [0]
            executeProgram([3,9,7,9,10,9,4,9,99,-1,8], 0, [7], []) `shouldBe` [1]
            executeProgram([3,9,7,9,10,9,4,9,99,-1,8], 0, [9], []) `shouldBe` [0]
            executeProgram([3,3,1108,-1,8,3,4,3,99], 0, [8], []) `shouldBe` [1]
            executeProgram([3,3,1108,-1,8,3,4,3,99], 0, [9], []) `shouldBe` [0]
            executeProgram([3,3,1107,-1,8,3,4,3,99], 0, [7], []) `shouldBe` [1]
            executeProgram([3,3,1107,-1,8,3,4,3,99], 0, [9], []) `shouldBe` [0]
            
        it "should jump correctly" $ do
            executeProgram([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0, [8], []) `shouldBe` [1]
            executeProgram([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], 0, [0], []) `shouldBe` [0]
            executeProgram([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0, [9], []) `shouldBe` [1]
            executeProgram([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], 0, [0], []) `shouldBe` [0]
        
        it "should be able to handle larger programs" $ do
            executeProgram([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 0, [7], []) `shouldBe` [999]
            executeProgram([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 0, [8], []) `shouldBe` [1000]
            executeProgram([3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99], 0, [9], []) `shouldBe` [1001]
