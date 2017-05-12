{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Fsm.Commons.Types where

import Data.Time
import Data.Data

-- defines types shared by different stages

newtype Limit = Limit Int deriving Num
newtype Timeout = Timeout Int
newtype Length = Length Double deriving (Num, Eq, Ord, Show)
newtype Weight = Weight Double deriving (Num, Eq, Ord, Show)
newtype PooFromFull = PooFromFull Int deriving Num
newtype PooFromSoSo = PooFromSoSo Int deriving Num

data Health = Dead | Sick | Weak | Fair | Healthy | Hardy deriving (Enum, Show, Eq)

data Input = IncreaseTemp | DecreaseTemp | Feed | Play | Medication | Bed | Sing deriving (Eq, Show, Ord)

type UserInput = (UTCTime, Maybe Input)

type UserPrompt = [Input] -> String -> IO UserInput

data Fullness = Full { fullnessTime :: UTCTime } |
                SoSo { fullnessTime :: UTCTime } |
                Hungry { fullnessTime :: UTCTime } deriving (Data, Eq, Show)

data PooAmount = PooAmount { pooTime :: UTCTime, poo :: Int } deriving (Eq, Show)

data Mood = Mood { moodTime :: UTCTime, moodValue :: Int} deriving (Eq, Show)

data Fatigue = Fatigue { fatigueTime :: UTCTime, fatigueValue :: Int } deriving (Eq, Show)

data Status = Sleeping { statusTime :: UTCTime } | Awake { statusTime :: UTCTime } deriving (Eq, Show)
