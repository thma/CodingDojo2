{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Health where

import           GHC.Generics    (Generic)              -- needed to derive type class instances declaratively     

data Employee = Employee {
  alcoholConsumption :: Double,
  healthAssessment :: Bool,
  healthControl :: Bool,
  smokingHabits :: SmokerType,
  bmi :: Double
} deriving (Show, Generic)

data SmokerType = Smoker | NonSmoker | QuittingSmoker deriving (Show, Generic)

reduction :: Employee -> Integer
reduction employee =
    let alcoholBonus = if (alcoholConsumption employee <= 20) then 30 else 0
        assessmentBonus = if (healthAssessment employee == True) then 25 else 0
        bmiBonus = if (bmi employee <= 27.5) 
                     then 50
                     else if (bmi employee <= 30) then 25 else 0
        smokingBonus = case (smokingHabits employee) of
                         Smoker -> -75 
                         NonSmoker -> 50
                         QuittingSmoker -> 25
        healthControlBonus = if (healthControl employee == True) then (bmiBonus + smokingBonus) else 0
    in alcoholBonus + assessmentBonus + healthControlBonus
                           
