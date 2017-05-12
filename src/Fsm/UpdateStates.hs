module Fsm.UpdateStates where

import Data.Time
import Fsm.Commons.Types
import Fsm.Commons
import Fsm.Utils

-- the StageUpdate func implementation are similar
-- among the different stages,
-- funcs in this module are defined for sharing among these stages

_medicate :: Health -> Health
_medicate health = decisionByHealth health Healthy health

_increaseMood :: Mood -> Limit -> UTCTime -> Mood
_increaseMood mood@(Mood _ v) (Limit limit) ct
  | v < limit = mood { moodValue = v + 1 }
  | otherwise = Mood ct (v + 1)

_decreaseMood :: Mood -> Limit -> Timeout -> UTCTime -> Mood
_decreaseMood mood@(Mood _ v) (Limit limit) (Timeout timeout) ct
  | age mood ct >= timeout && v <= limit = mood { moodValue = v - 1}
  | age mood ct >= timeout = Mood ct (v - 1)
  | otherwise = mood

_raiseFatigue :: Status -> Fatigue -> Limit -> Int -> Timeout -> UTCTime -> Fatigue
_raiseFatigue status fatigue@(Fatigue _ v) (Limit limit) maxAwakeTime (Timeout timeout) ct
  | (age status ct - maxAwakeTime) >= timeout && age fatigue ct >= timeout =
    let new = v + 1 in if v >= limit then fatigue { fatigueValue = new } else Fatigue ct new
  | otherwise = fatigue

_poop :: PooAmount -> Limit -> UTCTime -> PooAmount
_poop orig@(PooAmount _ amount) (Limit limit) ct
  | amount >= limit = PooAmount ct 0
  | otherwise = orig

_wakeup :: Status -> UTCTime -> Status
_wakeup (Sleeping _) = Awake
_wakeup awake = const awake

_feed :: Fullness -> UTCTime -> Fullness
_feed Hungry{} = SoSo
_feed SoSo{} = Full
_feed full = const full

data SleepAction = SleepAction { saFatigue :: Fatigue, saStatus :: Status }

-- sleep only if reach or > fatigueLimit
sleepIfTired :: SleepAction -> Limit -> UTCTime -> SleepAction
sleepIfTired orig@(SleepAction fatigue@(Fatigue _ v) Awake{}) (Limit limit) ct
  | v > limit = SleepAction fatigue { fatigueValue = v - 1} $ Sleeping ct
  | v == limit = SleepAction (Fatigue ct $ v - 1) $ Sleeping ct
sleepIfTired orig _ _ = orig

-- sleep no matter what fatigue value is, different from sleepIfTired
sleep :: SleepAction -> Limit -> UTCTime -> SleepAction
sleep orig@(SleepAction (Fatigue _ 0) Awake{}) _ ct = orig { saStatus = Sleeping ct }
sleep (SleepAction fatigue@(Fatigue _ v) Awake{}) (Limit limit) ct
  | v > limit = SleepAction fatigue { fatigueValue = v - 1} $ Sleeping ct
  | otherwise = SleepAction (Fatigue ct $ v - 1) $ Sleeping ct
sleep orig _ _ = orig -- impossibe to happen
