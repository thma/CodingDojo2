module StackSpec where

-- testing specific stuff
import           Test.Hspec       hiding (it)
import           TestUtils        (it)         
import Test.QuickCheck.Property   (property)

-- our test object
import           Stack

emptyIntStack :: Stack Int
emptyIntStack = stackNew


spec :: Spec
spec =
  describe "Stack" $ do
    it "constructs an empty stack" $
      stackIsEmpty stackNew `shouldBe` True
    it "can not pop from an empty stack" $
      stackPop emptyIntStack `shouldBe` Nothing
    it "pushes and popping is inverse" $
      property $ \n -> stackPop (stackPush emptyIntStack n) `shouldBe` Just (emptyIntStack, n)
    it "peek just gets the top of the stack" $
      property $ \n -> stackPeek (stackPush emptyIntStack n) `shouldBe` Just n
    it "can reverse a stack" $
      property $ \i j k -> stackPeek (stackReverse (stackPush (stackPush (stackPush emptyIntStack i) j) k)) `shouldBe` Just i  