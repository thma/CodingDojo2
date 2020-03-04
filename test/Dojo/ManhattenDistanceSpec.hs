module Dojo.ManhattenDistanceSpec
  (spec)
where

import           Control.Exception     (evaluate)
import           Test.Hspec            hiding (it)
import           Dojo.TestUtils        (it)         -- I'm redefining `it` to use 1000 examples
import           Test.QuickCheck

import           Lib

spec :: Spec
spec =
  describe "manhattanDistance" $ do

    it "computes the manhattanDistance of 0.0-1.1" $
     manhattanDistance(Point 0 0, Point 1 1) === 2

    it "computes the manhattanDistance of 1.1-1.2" $
     manhattanDistance(Point 1 1, Point 1 2) `shouldBe` 1

    it "computes the manhattanDistance of 1.1-2.2" $
     manhattanDistance(Point 1 1, Point 2 2) `shouldBe` 2
     
    it "computes the manhattanDistance of 2.2-5.4" $
     manhattanDistance(Point 2 2, Point 5 4) === 5
     
    it "computes the manhattanDistance of 5.4-2-2" $
     manhattanDistance(Point 5 4, Point 2 2) === 5


     
    it "computes the manhattanDistance of any x distance" $
      property $ \xDistance -> manhattanDistance(Point 0 0, Point xDistance 0) `shouldBe` (abs xDistance)