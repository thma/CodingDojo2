module Dojo.DojoSpec
  (spec)
where

import           Control.Exception     (evaluate)
import           Test.Hspec            hiding (it)
import           Dojo.TestUtils        (it)         -- I'm redefining `it` to use 1000 examples
import           Test.QuickCheck

import           Lib

spec :: Spec
spec = 
  describe "squared" $ do
    it "computes the square of 12" $
      squared 12 `shouldBe` 144
    it "computes the square of any Integer" $
      property $ \int -> squared int `shouldBe` (int * int :: Integer)
    it "computes the square of any Double" $
      property $ \double -> squared double `shouldBe` (double * double :: Double)

