module RpnSpec where

import           Test.Hspec            hiding (it)
import           TestUtils        (it)         

import           RpnCalculator

spec :: Spec
spec =
  describe "RPN Calculator" $ do
    it "1 -> 1" $
     rpn "1" `shouldBe` 1 
    it "22 -> 22" $
     rpn "22" `shouldBe` 22
    it "1 2 + -> 3" $
     rpn "1 2 +" `shouldBe` 3 
    it "20 5 / -> 4" $
     rpn "20 5 /" `shouldBe` 4
    it "20 5 * -> 100" $
     rpn "20 5 *" `shouldBe` 100
    it "20 5 - -> 15" $
     rpn "20 5 -" `shouldBe` 15
    it "4 2 + 3 - -> 3" $
     rpn "4 2 + 3 -" `shouldBe` 3     
    it "3 5 8 * 7 + * -> 141" $
     rpn "3 5 8 * 7 + *" `shouldBe` 141          
    it "9 SQRT -> 3" $
     rpn "9 SQRT" `shouldBe` 3  
    it "5 8 1 4 2 MAX -> 8" $
     rpn "5 8 1 4 2 MAX" `shouldBe` 8
    it "5 8 1 4 2 MAX 2 * -> 16" $
     rpn "5 8 1 4 2 MAX 2 *" `shouldBe` 16  
    it "computes 4 5 MAX 1 7 MAX 3 * -> 21" $
     rpn "4 5 MAX 1 7 MAX 3 *" `shouldBe` 21 