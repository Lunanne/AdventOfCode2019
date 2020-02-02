module UtilSpec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Util(manhattenDistance, Coordinate(..))

spec :: Spec
spec = do
    describe "Util methods" $ do
        it "should calculate manhatten between 2d coordinates correctly" $ do
            manhattenDistance(TwoDCoordinate 1 4, TwoDCoordinate 5 5) `shouldBe` 5
            manhattenDistance(TwoDCoordinate 1 4, TwoDCoordinate (-3) (-2)) `shouldBe` 10
            manhattenDistance(TwoDCoordinate (-1) (-4), TwoDCoordinate (-3) (-2)) `shouldBe` 4


