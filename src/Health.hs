--{-# LANGUAGE DeriveGeneric #-}

module Health where

import           GHC.Generics (Generic)

-- | This module implements a payment scheme calculation for a health insurance
--   https://twitter.com/NicolaLindgren/status/1571127878229831682
--   We are mainly interested in the proper setup of testcases for this algorithm.


data Employee = Employee
  { alcoholConsumption :: Double,
    healthAssessment   :: Bool,
    healthControl      :: Bool,
    smokingHabits      :: SmokerType,
    bmi                :: Double
  }
  deriving (Show, Generic)

data SmokerType = Smoker | NonSmoker | QuittingSmoker deriving (Show, Generic)

reduction :: Employee -> Integer
reduction employee =
  let alcoholBonus    = if alcoholConsumption employee <= 20 then 30 else 0
      assessmentBonus = if healthAssessment employee         then 25 else 0
      bmiBonus
        | (bmi employee <= 27.5) = 50
        | (bmi employee <= 30)   = 25
        | otherwise = 0
      smokingBonus = case smokingHabits employee of
        Smoker         -> -75
        NonSmoker      -> 50
        QuittingSmoker -> 25
      healthControlBonus = if healthControl employee then bmiBonus + smokingBonus else 0
   in alcoholBonus + assessmentBonus + healthControlBonus
