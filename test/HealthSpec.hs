module HealthSpec where

-- testing specific stuff
import           Test.Hspec       hiding (it)
import           TestUtils        (it)         
import Test.QuickCheck.Property   (property)
import Test.QuickC.heck
import Test.QuickCheck.Arbitrary.Generic

import           Health

-- emp = Employee {
--   alcoholConsumption = 10,
--   healthAssessment = True,
--   healthControl = True,
--   smokingHabits = NonSmoker,
--   bmi = 28
-- }

instance Arbitrary SmokerType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Employee where
  arbitrary = genericArbitrary
  shrink = genericShrink


spec :: Spec
spec =
  describe "Health behaviour based reduction" $ do
    it "should consider alcohol consumption" $
      property $ \emp -> do 
        print emp
        reduction emp - reduction (emp {alcoholConsumption = 50}) `shouldBe` 30
    -- it "can not pop from an empty stack" $
    --   stackPop emptyIntStack `shouldBe` Nothing
    -- it "pushes and popping is inverse" $
    --   property $ \n -> stackPop (stackPush emptyIntStack n) `shouldBe` Just (emptyIntStack, n)
    -- it "peek just gets the top of the stack" $
    --   property $ \n -> stackPeek (stackPush emptyIntStack n) `shouldBe` Just n
    -- it "can reverse a stack" $
    --   property $ \i j k -> stackPeek (stackReverse (stackPush (stackPush (stackPush emptyIntStack i) j) k)) `shouldBe` Just i  