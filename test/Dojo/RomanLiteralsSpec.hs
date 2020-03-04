module Dojo.RomanLiteralsSpec where

import           Test.Hspec            hiding (it)
import           Dojo.TestUtils        (it)         
import           Test.QuickCheck

import           RomanLiterals

spec :: Spec
spec =
  describe "RomanLiterals" $ do
    it "parses single literals" $
     mapM_
        (\(str, val) -> convert str `shouldBe` val)
        [("i",1),("v",5),("x", 10),("l",50),("c",100),("d",500),("m",1000)]
    it "parses cc" $
     convert "cc" `shouldBe` 200
    it "parses lx" $
     convert "lx" `shouldBe` 60
    it "parses xc" $
     convert "xc" `shouldBe` 90
    it "parses mcd" $
     convert "mcd" `shouldBe` 1400
    it "parses ccc" $
     convert "ccc" `shouldBe` 300
    it "parses MDCCCCLXXXIIII" $
      convert "MDCCCCLXXXIIII" `shouldBe` 1984
    it "parses MCMLXXXIV" $
      convert "MCMLXXXIV" `shouldBe` 1984


