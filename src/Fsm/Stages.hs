{-# LANGUAGE BangPatterns #-}

module Fsm.Stages where

import Data.Time
import Fsm.Commons
import Fsm.UpdateStates as US
import Fsm.Constants
import Fsm.Utils
import Data.Semigroup

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

instance Timeable Stage where
  age egg@Egg{} = const 0
  age pet = (secsSpent . bornTime) pet

getConstants :: Stage -> AllConstants -> StageConstants
getConstants Egg{} = eggConsts
getConstants Chicken{} = chickenConsts
getConstants Adult{} = adultConsts
getConstants Elder{} = elderConsts

-- a function shared by depressEffect, constipateEffect, fatigueEffect
-- because these funcs will downgrade upon timeout
weakenEffect :: WeakFactor a => (Stage -> a) -> (StageConstants -> Int) -> Stage -> UTCTime -> StageConstants -> (Stage, a)
weakenEffect getFactor getTimeout pet ct petConsts =
  let timeout = getTimeout petConsts
      weakFactor = getFactor pet in
  if reachLimit weakFactor petConsts && age weakFactor ct >= timeout then
    (pet { health = (downgrade . health) pet }, renew weakFactor ct)
  else (pet, weakFactor)

-- a function shared by playEffect, digestEffect
digest :: Stage -> StageConstants -> Weight -> UTCTime -> Stage
digest egg@Egg{} _ _ _ = egg -- won't happen, just for completeness
digest pet consts wtGain ct =
  let !_fullness = fullness pet
      updateFuncs pFun wtFun lFun =
          let [deltaPooFull, deltaPooSoSo, limit] = [pooFromFull, pooFromSoSo, pooLimit] <*> [consts]
              !wtLoss = (Weight . weightLoss) consts
              deltaLenFull = (Length . lengthFromFull) consts
              !pLimit = Limit limit in
          case _fullness of Full{} -> (pFun deltaPooFull pLimit, wtFun wtGain, lFun deltaLenFull)
                            SoSo{} -> (pFun deltaPooSoSo pLimit, (`wtFun` wtLoss), lFun 0)
                            Hungry{} -> (pFun 0 pLimit, (`wtFun` wtLoss), lFun 0)
      (updateP, updateW, updateL) = updateFuncs (pooAfter _fullness ct) (weightAfter _fullness) (lengthAfter _fullness) in
  pet { petLength = (updateL . petLength) pet,
        weight = (updateW . weight) pet,
        fullness = decreaseFullness _fullness ct,
        pooAmt = (updateP . pooAmt) pet,
        health = (healthAfter _fullness . health) pet }

-- a func to be shared by sleep and sleepIfTired funcs
sleepTemplate :: (SleepAction -> Limit -> UTCTime -> SleepAction) -> StageUpdate
sleepTemplate _ egg@Egg{} _ _ = egg
sleepTemplate f pet ct petConsts =
  let SleepAction _fatigue _status = f (SleepAction (fatigue pet) (status pet)) ((Limit . fatigueLimit) petConsts) ct in
  pet { fatigue = _fatigue, status = _status}

-- determines what inputs are allowed to be entered by the user
-- according to the given stage and its states
-- such as stutus, fullness, health
inputsByState :: Stage -> [Input]
inputsByState egg@Egg{} = eggInputs
inputsByState pet =
  let (inps, funcs) = case pet of Chicken{} -> (chickenInputs, [])
                                  Adult{} -> (adultInputs, [])
                                  Elder _ _ _ _ _ _ _ _ _ _ 0 -> (elderInputs, [filter $ \inp -> inp /= Medication])
                                  _ -> (elderInputs, []) in
  updateListByFuncs inps $ ([inputsByStatus . status, inputsByFullness . fullness, inputsByHealth . health] <*> [pet]) ++ funcs

-- type alias for the type of functions that update the stage to another stage
-- according to the given currentTime and configured values from appl.cfg
type StageUpdate = COMPOSE_FT Stage UTCTime StageConstants

autoUpdate :: StageUpdate
autoUpdate pet = composeFuncs xs pet
  where xs = case pet of Egg{} -> []
                         Chicken{} -> chickenAutoUpdates
                         Adult{} -> adultAutoUpdates
                         Elder{} -> elderAutoUpdates

-- difference between sleepIfTired, sleep can be referred
-- to their correspondent in UpdateStates.hs
sleepIfTired :: StageUpdate
sleepIfTired = sleepTemplate US.sleepIfTired

sleep :: StageUpdate
sleep = sleepTemplate US.sleep

-- update the stage after playing
playEffect :: StageUpdate
playEffect egg@Egg{} _ _ = egg
playEffect pet1 ct petConsts =
  let pet2 = digest pet1 petConsts (Weight 0.0) ct in
  pet2 { petLength = petLength pet2 + (Length . lengthFromPlay) petConsts,
         mood = _increaseMood (mood pet2) ((Limit . depressIndex) petConsts) ct,
         health = (upgradeHealth . health) pet2 }

-- update the stage after digestion which will happen if its latest fullness
-- state has passed digestSecs configured in appl.cfg
digestEffect :: StageUpdate
digestEffect egg@Egg{} _ _ = egg
digestEffect pet ct petConsts =
  if age (fullness pet) ct < digestSecs petConsts then pet
  else digest pet petConsts (Weight $ weightFromFull petConsts) ct

-- update the stage after singing
sing :: StageUpdate
sing adult@Adult{} ct petConsts =
  let limit = (Limit . depressIndex) petConsts in
  adult { mood = _increaseMood (mood adult) limit ct }
sing otherStage _ _ = otherStage

-- update the stage after medicate
medicate :: StageUpdate
medicate egg@Egg{} _ _ = egg
medicate elder@(Elder _ 0 _ _ _ _ _ _ _ _ _) _ _ = elder
medicate elder@Elder{} _ _ = elder { medAllows = medAllows elder - 1, health = (_medicate . health) elder }
medicate pet _ _ = pet { health = (_medicate . health) pet }

-- update the stage after feed
feed :: StageUpdate
feed egg@Egg{} _ _ = egg
feed pet ct _ = pet { fullness = _feed (fullness pet) ct }

-- update the stage after wakeup
wakeup :: StageUpdate
wakeup egg@Egg{} _ _ = egg
wakeup pet ct _ = pet { status = _wakeup (status pet) ct }

-- update the stage after poop
poop :: StageUpdate
poop egg@Egg{} _ _ = egg
poop pet ct petConsts =
  let limit = (Limit . pooLimit) petConsts in
  pet { pooAmt = _poop (pooAmt pet) limit ct }

-- grow to next Stage according to its current stage and
-- if it meet the maturity condition
grow :: StageUpdate
grow chick@(Chicken bornTime _length weight fullness pooAmt mood status fatigue health) ct chickenConsts
  | age chick ct < duration chickenConsts = chick
  | otherwise = Adult bornTime _length weight fullness pooAmt mood status fatigue health
grow adult@(Adult bornTime _length weight fullness pooAmt mood status fatigue health) ct adultConsts
  | age adult ct < duration adultConsts = adult
  | otherwise = Elder bornTime _length weight fullness pooAmt mood status fatigue health ct $ medAllowsForElder adultConsts
grow otherStage _ _ = otherStage

-- if the stage is Elder, it will be weakerWithTime
weakerWithTime :: StageUpdate
weakerWithTime elder@(Elder _ _ _ _ _ _ _ _ health time _) ct elderConsts
  | secsSpent time ct < weakerInSecs elderConsts = elder
  | otherwise = elder { weakerSince = ct, health = downgrade health }
weakerWithTime pet _ _ = pet

-- raise fatigue value if it's awaken for > maxAwakeSecs configured in appl.conf
raiseFatigue :: StageUpdate
raiseFatigue egg@Egg{} _ _ = egg
raiseFatigue pet ct petConsts =
  let [limit, _maxAwakeSecs, timeout] = [fatigueLimit, maxAwakeSecs, raiseFatigueSecs] <*> [petConsts]
      newFatigue = _raiseFatigue (status pet) (fatigue pet) (Limit limit) _maxAwakeSecs (Timeout timeout) ct in
  pet { fatigue = newFatigue }

-- decrease mood upon timeout of decreaseMoodSecs configured in appl.conf
decreaseMood :: StageUpdate
decreaseMood egg@Egg{} _ _ = egg
decreaseMood pet ct petConsts =
  let [depIndex, timeout] = [depressIndex, decreaseMoodSecs] <*> [petConsts]
      newMood = _decreaseMood (mood pet) (Limit depIndex) (Timeout timeout) ct in
  pet { mood = newMood }

-- downgrade health if it has been depressed for > depressSecs in appl.cfg
depressEffect :: StageUpdate
depressEffect egg@Egg{} _ _ = egg
depressEffect pet ct petConsts =
  let (newPet, newMood) = weakenEffect mood depressSecs pet ct petConsts in
  newPet { mood = newMood }

-- downgrade health if it has been constipated for > pooLimitSecs in appl.cfg
constipateEffect :: StageUpdate
constipateEffect egg@Egg{} _ _ = egg
constipateEffect pet ct petConsts =
  let (newPet, newPoo) = weakenEffect pooAmt pooLimitSecs pet ct petConsts in
  newPet { pooAmt = newPoo }

-- downgrade health if it has been fatigue for > fatigueSecs in appl.cfg
fatigueEffect :: StageUpdate
fatigueEffect egg@Egg{} _ _ = egg
fatigueEffect pet ct petConsts =
  let (newPet, newFatigue) = weakenEffect fatigue fatigueSecs pet ct petConsts in
  newPet { fatigue = newFatigue }

-- madatory StageUpate funcs to be applied to the pet when its current Stage is Chicken
!chickenAutoUpdates = [digestEffect, constipateEffect, decreaseMood, depressEffect, raiseFatigue, fatigueEffect]
-- madatory StageUpate funcs to be applied to the pet when its current Stage is Adult
!adultAutoUpdates = chickenAutoUpdates
-- madatory StageUpate funcs to be applied to the pet when its current Stage is Elder
!elderAutoUpdates = chickenAutoUpdates ++ [weakerWithTime]

-- present the stage as output msg
present :: Stage -> AllConstants -> UTCTime -> String
present Egg{} _ _ = ""
present pet allConsts ct =
  case pet of Elder{} -> listToString $ labels ++
                         show (medAllows pet) <> " medication remains" :
                         mandatoryMsgs ++
                         ["The health may be weakened " <>
                          show ((weakerInSecs . elderConsts) allConsts - secsSpent (weakerSince pet) ct) <>
                          " secs later."]
              _ -> listToString $ labels ++ mandatoryMsgs
    where labels = ["Latest status", "============="]
          mandatoryMsgs =
            let !consts = getConstants pet allConsts
                [_depressIndex, _happyIndex, _depressSecs, _maxAwakeSecs, _fatigueLimit, _fatigueSecs, _digestSecs, _pooLimit, _pooLimitSecs] =
                  [depressIndex, happyIndex, depressSecs, maxAwakeSecs, fatigueLimit, fatigueSecs, digestSecs, pooLimit, pooLimitSecs] <*> [consts] in
            [const . presentLength . petLength,
             const . presentWeight . weight,
             \p -> const $ presentBmi (weight p) (petLength p),
             presentFullness _digestSecs . fullness,
             presentPooAmount (Limit _pooLimit) _pooLimitSecs . pooAmt,
             presentMood _depressIndex _happyIndex _depressSecs . mood,
             presentStatus _maxAwakeSecs . status,
             presentFatigue (Limit _fatigueLimit) _fatigueSecs . fatigue,
             const . presentHealth . health
            ] <*> [pet] <*> [ct]
