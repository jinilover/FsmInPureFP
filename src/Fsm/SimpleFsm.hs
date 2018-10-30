{-# LANGUAGE RecordWildCards #-}
module Fsm.SimpleFsm
  ( transit )
  where

import Fsm.Stages.Types
import Fsm.Stages.Egg
import Fsm.Stages
import Fsm.Constants.Types
import Fsm.Commons.Types
import Fsm.Commons 
import Fsm.Utils
import Data.Data
import Data.Time
import Data.Semigroup
import Control.Arrow

-- prompt for user input and apply the input (aka event in the FSM)
-- to the pet's current state (aka stage)
transit :: Stage -> UserPrompt -> [Input] -> String -> AllConstants -> IO String
transit stage prompt inputs msg allConsts = do
  (ct, inp) <- prompt inputs msg
  process stage prompt (ct, inp) allConsts

-- given the current stage and the user input and perform the update accordingly
process :: Stage -> UserPrompt -> UserInput -> AllConstants -> IO String
-- Egg --
-- process egg@Egg{} _ (_, input) _
--   | currTemp egg == 100 = return $ "Testing!! " <> "Input is " <> show input
-- cooked the egg
process Egg{..} _ (_, Just IncreaseTemp) allConsts
  | currTemp == (fatalMaxTemp . eggConsts) allConsts =
    return "The egg has reached the max temperature, you've cooked it"
-- frozen the egg
process Egg{..} _ (_, Just DecreaseTemp) allConsts
  | currTemp == (fatalMinTemp . eggConsts) allConsts =
    return "The egg has reached the min temperature, you've frozen it"
-- egg suffered the fatal temps for long time
process (Egg chgdTime temp _ Sick) _ (ct, _) allConsts
  | secsSpent chgdTime ct >= _fatalTempSecs && isFatalTemp temp allConsts =
    return $
      "The egg is in poor health and suffered the " <> extreme <> " temperature for at least "
      <> show _fatalTempSecs <> " secs, failed to hatch"
    where [_fatalTempSecs, _fatalMaxTemp] = ($ eggConsts allConsts) <$> [fatalTempSecs, fatalMaxTemp]
          extreme = if temp == _fatalMaxTemp then "max" else "min"
-- hatch
process egg@(Egg chgdTime temp energyToHatch h) prompt (ct, _) allConsts
  | energySpent chgdTime ct temp >= energyToHatch =
      let newHealth = updateEggHealth chgdTime ct temp allConsts h
          chicken = Chicken {
                      bornTime = ct,
                      petLength = (Length . lengthByHealth allConsts) newHealth,
                      weight = (Weight . weightByHealth allConsts) newHealth,
                      fullness = SoSo ct,
                      pooAmt = PooAmount ct 0,
                      mood = Mood ct 0,
                      status = Awake ct,
                      fatigue = Fatigue ct 0,
                      health = newHealth } in
          transit chicken prompt (inputsByState chicken) (transitMsg egg chicken ct allConsts) allConsts
-- change temp or no input
process egg@(Egg chgdTime temp energyToHatch h) prompt (ct, inp) allConsts =
  let newHealth = updateEggHealth chgdTime ct temp allConsts h
      newTemp = case inp of Just IncreaseTemp -> temp + 1
                            Just DecreaseTemp -> temp - 1
                            _                 -> temp
      newEgg = if newTemp == temp && newHealth == h then egg else
               Egg ct newTemp (energyToHatch - energySpent chgdTime ct temp) newHealth in
  transit newEgg prompt (inputsByState newEgg) (transitMsg egg newEgg ct allConsts) allConsts
-- Chicken, adult or elder --
-- No input happens on Sleeping according to inputsByStatus
process pet prompt (ct, _) allConsts
  | isSleeping $ status pet = handleSleeping pet prompt ct allConsts
-- No input happens on non-Sleeping due to no or invalid input
process pet prompt (ct, Nothing) allConsts =
  handleNoInput pet prompt ct allConsts
-- Feed only happens to non-Full according to inputsByFullness
process pet prompt (ct, Just Feed) allConsts =
  handleFeed pet prompt ct allConsts
-- Play only happens to non-Sick according to inputsByHealth
process pet prompt (ct, Just Play) allConsts =
  handlePlay pet prompt ct allConsts
-- Medication only happens on Sick according to inputsByHealth
process pet prompt (ct, Just Medication) allConsts =
  handleMedicate pet prompt ct allConsts
-- Bed
process pet prompt (ct, Just Bed) allConsts
  | (not . isSleeping . status) pet = handleBed pet prompt ct allConsts
-- Adult
-- Sing only happens on Adult according to inputsByState
process pet@Adult{} prompt (ct, Just Sing) allConsts =
  handleSing pet prompt ct allConsts

transitMsg :: Stage -> Stage -> UTCTime -> AllConstants -> String
transitMsg (Egg time1 temp1 _ h1) (Egg time2 temp2 energyToHatch2 h2) ct allConsts =
  listToString [
    if time1 == time2 then "" else "Temperature or health changed since " <> format time2,
    if h1 == h2 then show h2 else "Health downgraded to " <> show h2,
    if temp2 == fatalMinTemp _eggConsts then warning "min" "decrease" else "",
    if temp2 == fatalMaxTemp _eggConsts then warning "max" "increase" else "",
    show (secsRqd - secsSpent time2 ct) <> " more secs to hatch for temperature " <> show temp2 <> " degrees"
  ]
  where _eggConsts = eggConsts allConsts
        warning issue fatalAction =
          "Temperature has reached fatal " <> issue <> " " <> show temp2 <> " degrees, " <> "further " <>
          fatalAction <> " or leave it for " <> show (fatalTempSecs _eggConsts) <> " secs will kill the egg!"
        secsRqd = let division = energyToHatch2 `div` temp2 in
                  if energyToHatch2 `mod` temp2 == 0 then division else division + 1
transitMsg Egg{} chicken ct allConsts =
  "Hatched to a chicken!\n" <> present chicken allConsts ct
transitMsg old new ct allConsts =
  let toTuple = (head &&& last) . (<$> [old, new])
      lengths = toTuple petLength
      weights = toTuple weight
      statuses = toTuple status
      healths = toTuple health
      [p1, p2, m1, m2, f1, f2] =
        [poo . pooAmt, moodValue . mood, fatigueValue . fatigue] <*> [old, new]
      func name =
        stateChgdMsg name (bornTime new) ct lengths weights (p1, p2) (m1, m2) statuses (f1, f2) healths
        <> present new allConsts ct in
  case (old, new) of (Chicken{}, Chicken{}) -> func "A chicken"
                     (Chicken{}, Adult{}) -> func "Grown up as ADULT!"
                     (Adult{}, Adult{}) -> func "An adult"
                     (Adult{}, Elder{}) -> func "Retired as ELDER!"
                     _ -> func "An elder"

type Handler = Stage -> UserPrompt -> UTCTime -> AllConstants -> IO String

-- a helper function be shared by the handleXXX functions
-- 1st arg is the original Stage
-- 2nd arg is the currentTime
-- 3rd arg is the configured constant values from appl.cfg
-- 4th arg is the StageUpdate funcs must be included
-- 5th arg is the StageUpdate funcs that maybe included by according to the "random" decision function
-- 6th arg is the StageUpdate funcs must be included
-- the sequence of the StageUpdate is important, that's why the arguments are arranged in this way
updateForHandler :: Stage -> UTCTime -> AllConstants -> [StageUpdate] -> [StageUpdate] -> [StageUpdate] -> Stage
updateForHandler old ct allConsts reqdUpdates1 optUpdates reqdUpdates2 =
  composeFuncs (includeOptUpdates ++ reqdUpdates2 ++ [grow]) old ct $ getConstants old allConsts
  where includeOptUpdates = [composeRandomFuncs ct (autoUpdate:reqdUpdates1) optUpdates]

handleSleeping :: Handler
handleSleeping stage prompt ct allConsts
  | age _status ct < _sleepSecs =
      let msg = "Still sleeping, need to sleep " <> show (timeRemains _status ct _sleepSecs) <> " more secs" in
      nextPetState stage (newState [] [] []) msg prompt ct allConsts
  | otherwise = nextPetState stage (newState [wakeup] [poop] []) "Woke up" prompt ct allConsts
    where _sleepSecs = (sleepingSecs . getConstants stage) allConsts
          _status = status stage
          newState = updateForHandler stage ct allConsts

handleBed :: Handler
handleBed stage prompt ct allConsts
  | age _status ct < _awakeSecs =
      let msg = "Wake for " <> show (timeRemains _status ct _awakeSecs)
                <> " secs only, < allowed " <> show _awakeSecs <> " secs, won't sleep!" in
      nextPetState stage (newState [] [poop] []) msg prompt ct allConsts
  | otherwise = nextPetState stage (newState [] [poop] [sleep]) "Went sleeping" prompt ct allConsts
    where _awakeSecs = (maxAwakeSecs . getConstants stage) allConsts
          _status = status stage
          newState = updateForHandler stage ct allConsts

handleNoInput :: Handler
handleNoInput stage prompt ct allConsts =
  nextPetState stage (newState [] [poop, sleepIfTired] []) "No user input"  prompt ct allConsts
  where newState = updateForHandler stage ct allConsts

handleFeed :: Handler
handleFeed stage prompt ct allConsts =
  nextPetState stage (newState [feed] [poop, sleepIfTired] []) "It's fed" prompt ct allConsts
  where newState = updateForHandler stage ct allConsts

handleMedicate :: Handler
handleMedicate stage prompt ct allConsts =
  nextPetState stage (newState [medicate] [poop, sleepIfTired] []) "It's medicated" prompt ct allConsts
  where newState = updateForHandler stage ct allConsts

handlePlay :: Handler
handlePlay stage prompt ct allConsts =
  nextPetState stage (newState [playEffect] [poop, sleepIfTired] []) "Finished playing" prompt ct allConsts
  where newState = updateForHandler stage ct allConsts

handleSing :: Handler
handleSing stage prompt ct allConsts =
  nextPetState stage (newState [sing] [poop, sleepIfTired] []) "Sang a song" prompt ct allConsts
  where newState = updateForHandler stage ct allConsts

nextPetState :: Stage -> Stage -> String -> UserPrompt -> UTCTime -> AllConstants -> IO String
nextPetState old new actionDone prompt ct allConsts
  | health new == Dead = gameOver new allConsts ct
  | otherwise = transit new prompt (inputsByState new)
      (actionDone <> "\n" <> transitMsg old new ct allConsts)
      allConsts

gameOver :: Stage -> AllConstants -> UTCTime -> IO String
gameOver pet allConsts ct = return $
  case pet of Egg{} -> "It's impossible to be an egg, bug in your program!"
              Chicken{} -> helper "The chicken cannot grow as adult.  You fail to take care of it."
              Adult{} -> helper "The adult cannot proceed to elder.  You fail to take care of it."
              Elder _ _ _ _ _ _ _ _ _ _ n ->
                helper $ "It's dead.  " <> if n == 0 then "It used up medication." else show n <> " medication remains."
    where helper stageSpecific =
            listToString [
              stageSpecific,
              "It has lived for " <> show (age pet ct) <> " secs,",
              (presentLength . petLength) pet,
              (presentWeight . weight) pet,
              presentBmi (weight pet) (petLength pet),
              timeableMsg "" (toConstr . fullness) fullness,
              timeableMsg "has poo " (poo . pooAmt) pooAmt,
              timeableMsg "mood index is " (moodValue . mood) mood,
              statusMsg,
              timeableMsg "fatigue index is " (fatigueValue . fatigue) fatigue]
          statusMsg =
            let _status = status pet
                sTime = statusTime _status
                trail = " for " <> show (secsSpent sTime ct) <> " secs since " <> show (format sTime) in
            case _status of Awake{} -> "Awake" <> trail
                            _ -> "Slept" <> trail
          timeableMsg prefix toShow toTimeable =
            prefix <> (show . toShow) pet <> " for " <> show (toTimeable pet `age` ct) <> " secs."
