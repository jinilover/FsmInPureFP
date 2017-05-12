module Fsm.Stages.Types where

import Data.Time
import Fsm.Commons.Types

data Stage = Egg { stateChgdTime :: UTCTime,
                   currTemp :: Int ,
                   energyToHatch :: Int,
                   health :: Health } |
             Chicken { bornTime :: UTCTime,
                       petLength :: Length,
                       weight :: Weight,
                       fullness :: Fullness,
                       pooAmt :: PooAmount,
                       mood :: Mood,
                       status :: Status,
                       fatigue :: Fatigue,
                       health :: Health} |
             Adult { bornTime :: UTCTime,
                     petLength :: Length,
                     weight :: Weight,
                     fullness :: Fullness,
                     pooAmt :: PooAmount,
                     mood :: Mood,
                     status :: Status,
                     fatigue :: Fatigue,
                     health :: Health} |
             Elder { bornTime :: UTCTime,
                     petLength :: Length,
                     weight :: Weight,
                     fullness :: Fullness,
                     pooAmt :: PooAmount,
                     mood :: Mood,
                     status :: Status,
                     fatigue :: Fatigue,
                     health :: Health,
                     weakerSince :: UTCTime,
                     medAllows :: Int} deriving (Eq, Show)
