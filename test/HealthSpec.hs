module HealthSpec where

-- testing specific stuff
import           Test.Hspec       hiding (it)
import           TestUtils        (it)         
import Test.QuickCheck.Property   (property)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic

import           Health

emp = Employee {
  alcoholConsumption = 50,
  healthAssessment = False,
  healthControl = False,
  smokingHabits = Smoker,
  bmi = 50
}

instance Arbitrary SmokerType where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Employee where
  arbitrary = genericArbitrary
  shrink = genericShrink


spec :: Spec
spec =
  describe "Health behaviour based reduction" $ do
    it "does consider alcohol consumption" $
      reduction (emp {alcoholConsumption = 10}) - reduction emp  `shouldBe` 30
    it "does consider health assessment" $
      reduction (emp {healthAssessment = True}) - reduction emp  `shouldBe` 25
    it "does consider annual health control and BMI" $
      reduction (emp {healthControl = True}) - reduction (emp {healthControl = True, bmi=21}) - reduction (emp {healthControl = True, bmi=28})  `shouldBe` 0
    it "does consider annual health control and smoking habits" $
      reduction (emp {healthControl = True, smokingHabits=NonSmoker}) - reduction (emp {healthControl = True, smokingHabits=QuittingSmoker})  `shouldBe` 25

