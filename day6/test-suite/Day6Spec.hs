module Day6Spec (spec) where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import Day6 (part1, part2)

spec :: Spec
spec = do
    describe "Day 6 Part 1" $ do
        it "should count the connections" $ do
            part1 ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L"] `shouldBe` 42
        
    describe "Day 6 Part 2" $ do
        it "should count the distance between me and santa" $ do
            part2 ["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L", "K)YOU","I)SAN"] `shouldBe` 4
           
    