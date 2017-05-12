module Fsm.Stages.Egg where

import Fsm.Utils
import Fsm.Commons
import Fsm.Commons.Types
import Fsm.Constants.Types
import Data.Time

energySpent :: UTCTime -> UTCTime -> Int -> Int
energySpent ownTime ct temp = secsSpent ownTime ct * temp

updateEggHealth :: UTCTime -> UTCTime -> Int -> AllConstants -> Health -> Health
updateEggHealth tempChgdTime ct temp allConsts
  | isFatalTemp temp allConsts && secsSpent tempChgdTime ct >= (fatalTempSecs . eggConsts) allConsts = downgrade
  | otherwise = id

lengthByHealth :: AllConstants -> Health -> Double
lengthByHealth allConsts = (`proRata` (baseLength . eggConsts) allConsts)

weightByHealth :: AllConstants -> Health -> Double
weightByHealth allConsts = (`proRata` (baseWeight . eggConsts) allConsts)

proRata :: Health -> Double -> Double
proRata h = let xs = map (realToFrac . fromEnum) [h, Hardy] in
            (*) (1.0 + head xs / last xs)
