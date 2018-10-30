module Fsm.Commons
  ( upgradeHealth
  , presentBmi
  , presentLength
  , presentWeight
  , presentFullness
  , presentPooAmount
  , presentMood
  , presentStatus
  , presentFatigue
  , presentHealth
  , age
  , decisionByHealth
  , Timeable
  , timeRemains
  , renew
  , reachLimit
  , pooAfter
  , weightAfter
  , lengthAfter
  , healthAfter
  , WeakFactor
  , isFatalTemp
  , decreaseFullness
  , isSleeping
  , inputsByStatus
  , inputsByFullness
  , inputsByHealth
  , eggInputs
  , chickenInputs
  , adultInputs
  , elderInputs
  , noDisturbInputs
  , stateChgdMsg )
where

-- defines functions shared by different stages

import Data.Time
import Data.Semigroup
import Fsm.Utils
import Fsm.Constants.Types
import Fsm.Commons.Types

upgradeHealth :: Health -> Health
upgradeHealth Sick = Sick
upgradeHealth Dead = Dead
upgradeHealth Hardy = Hardy
upgradeHealth h = succ h

bmi :: Weight -> Length -> Double
bmi (Weight w) (Length l) = w / (l * l)

presentBmi :: Weight -> Length -> String
presentBmi w l = "BMI " <> roundup 5 (bmi w l)

presentLength :: Length -> String
presentLength (Length l) = roundup 2 l <> "cm tall"

presentWeight :: Weight -> String
presentWeight (Weight w) = roundup 2 w <> "g"

presentFullness :: Int -> Fullness -> UTCTime -> String
presentFullness _ Full{} _ = "Full"
presentFullness _ SoSo{} _ = "Neither full or hungry"
presentFullness timeout hungry@Hungry{} ct =
  "Hungry for " <> show (age hungry ct) <> " secs since " <> (format . fullnessTime) hungry <>
  ", hungry for " <> show timeout <> " secs will worsen the health, suggest to feed it"

presentPooAmount :: Limit -> Int -> PooAmount -> UTCTime -> String
presentPooAmount (Limit limit) timeout pooAmt@(PooAmount _ amt) ct
  | amt >= limit = mandatoryMsg <> ", constipated for " <> show (timeRemains pooAmt ct timeout) <>
                   " more secs will worsen the health, let's see if it will poop itself"
  | otherwise = mandatoryMsg
  where mandatoryMsg = "Accumulated " <> show amt <> " poo for " <> show (age pooAmt ct) <>
                        " secs since " <> (format . pooTime) pooAmt

presentMood :: Int -> Int -> Int -> Mood -> UTCTime -> String
presentMood depressIndex happyIndex timeout mood@(Mood _ v) ct
  | v <= depressIndex = mandatoryMsg <> "depressed for " <> show timeout <>
                        " secs will worsen the health, suggest to play with it"
  | v >= happyIndex = mandatoryMsg <> "it's happy, that's great!"
  | otherwise = mandatoryMsg <> "not bad"
  where mandatoryMsg = "Mood is " <> show v <> " for " <> show (age mood ct) <>
                       " secs since " <> (format . moodTime) mood <> ", "

presentStatus :: Int -> Status -> UTCTime -> String
presentStatus _ Sleeping{} _ = "Sleeping"
presentStatus maxAwakeSecs status@Awake{} ct
  | maxAwakeSecs > secsAwaken =
      mandatoryMsg <> "after " <> show (timeRemains status ct maxAwakeSecs) <> " secs"
  | otherwise = mandatoryMsg <> "now"
  where mandatoryMsg = "Awake for " <> show secsAwaken <> " secs since " <>
                        (format . statusTime) status <> ", it needs a rest "
        secsAwaken = age status ct

presentFatigue :: Limit -> Int -> Fatigue -> UTCTime -> String
presentFatigue (Limit limit) timeout f@(Fatigue _ v) ct
  | v >= limit = mandatoryMsg <> "fatigue for " <> show timeout <>
                 " secs will worsen the health, suggest to put it to bed or see if it will sleep itself"
  | otherwise = mandatoryMsg <> "not bad"
  where mandatoryMsg = "Fatigue is " <> show v <> " for " <> show (age f ct) <>
                       " secs since " <> (format . fatigueTime) f <> ", "

presentHealth :: Health -> String
presentHealth Sick = "It's sick, needs medication"
presentHealth Weak = "It's weak, needs improvement"
presentHealth Fair = "Fair health, not bad"
presentHealth Healthy = "Healthy!  Keep going!"
presentHealth Hardy = "Hardy!  No one can fight with you"
presentHealth _ = "It shouldn't happen!  Bug in your program"

class Timeable a where
  age :: a -> UTCTime -> Int

class Timeable a => WeakFactor a where
  renew :: a -> UTCTime -> a
  reachLimit :: a -> StageConstants -> Bool

class Timeable a => Countdown a where
  timeRemains :: a -> UTCTime -> Int -> Int

class Digestive a where
  pooAfter :: a -> UTCTime -> Int -> Limit -> PooAmount -> PooAmount
  weightAfter :: a -> Weight -> Weight -> Weight
  lengthAfter :: a -> Length -> Length -> Length
  healthAfter :: a -> Health -> Health

instance Timeable Fullness where
  age = secsSpent . fullnessTime

instance Digestive Fullness where
  pooAfter Hungry{} _ _ _ = id
  pooAfter _ ct delta (Limit pooLimit) = increasePoo
    where increasePoo orig@(PooAmount _ amount) =
            let newAmt = amount + delta in
            if amount >= pooLimit then orig {poo = newAmt} else PooAmount ct newAmt

  weightAfter Full{} = (+)
  weightAfter _ = (-)

  lengthAfter Full{} = (+)
  lengthAfter _ = const id

  healthAfter Full{} = upgradeHealth
  healthAfter SoSo{} = id
  healthAfter _ = downgrade

instance Timeable PooAmount where
  age = secsSpent . pooTime

instance WeakFactor PooAmount where
  renew p ct = p { pooTime = ct}
  reachLimit p consts = poo p >= pooLimit consts

instance Countdown PooAmount where
  timeRemains pooAmt ct secs = secs - age pooAmt ct

instance Timeable Mood where
  age = secsSpent . moodTime

instance WeakFactor Mood where
  renew m ct = m { moodTime = ct }
  reachLimit m consts = moodValue m <= depressIndex consts

instance Timeable Fatigue where
  age = secsSpent . fatigueTime

instance WeakFactor Fatigue where
  renew f ct = f { fatigueTime = ct}
  reachLimit f consts = fatigueValue f >= fatigueLimit consts

instance Timeable Status where
  age = secsSpent . statusTime

instance Countdown Status where
  timeRemains status ct secs = secs - age status ct

-- if it's sick, return the 2nd arg, o.w. the 3rd one
decisionByHealth :: Health -> a -> a -> a
decisionByHealth Sick sickAction = const sickAction
decisionByHealth _ _ = id

decreaseFullness :: Fullness -> UTCTime -> Fullness
decreaseFullness Full{} = SoSo
decreaseFullness SoSo{} = Hungry
decreaseFullness hungry = const hungry

isFatalTemp :: Int -> AllConstants -> Bool
isFatalTemp temp allConsts = elem temp $ ($ eggConsts allConsts) <$> [fatalMinTemp, fatalMaxTemp]

isSleeping :: Status -> Bool
isSleeping Sleeping{} = True
isSleeping _ = False

-- if it's sleeping, no input allowed from user to avoid disturbing
inputsByStatus :: Status -> [Input] -> [Input]
inputsByStatus Sleeping{} = const noDisturbInputs
inputsByStatus _ = id

-- if it's full, no "feed" allowed from user
inputsByFullness :: Fullness -> [Input] -> [Input]
inputsByFullness Full{} = filter $ \x -> x /= Feed
inputsByFullness _ = id

-- if it's sick, no "play" allowed, o.w. no "medicate" allowed
inputsByHealth :: Health -> [Input] -> [Input]
inputsByHealth h xs = decisionByHealth h [x |x <- xs, x /= Play] [x |x <- xs, x /= Medication]

-- inputs allowed by the user when the stage is an egg
eggInputs = [IncreaseTemp, DecreaseTemp]
-- all possible inputs by the user when the stage is a chicken
chickenInputs = [Feed, Play, Medication, Bed]
-- all possible inputs by the user when the stage is an adult
adultInputs = Sing : chickenInputs
-- all possible inputs by the user when the stage is an elder
elderInputs = chickenInputs
noDisturbInputs = []

-- present all changed state of the current stage as output msg to the user
stateChgdMsg :: String ->
                UTCTime ->
                UTCTime ->
                (Length, Length) ->
                (Weight, Weight) ->
                (Int, Int) ->
                (Int, Int) ->
                (Status, Status) ->
                (Int, Int) ->
                (Health, Health) ->
                String
stateChgdMsg nextStageName initTime ct (l1, l2) (w1, w2) (p1, p2) (m1, m2) (s1, s2) (f1, f2) (h1, h2) =
  listToString [
    nextStageName <> " of age " <> show (secsSpent initTime ct) <> " secs",
    valChgdMsg l1 l2 "" "it is taller",
    valChgdMsg w1 w2 "it is lighter" "it is heavier",
    valChgdMsg p1 p2 "Done pooping" "",
    valChgdMsg m1 m2 "it is more upset :(" " it is happier :)",
    statusMsg s1 s2,
    valChgdMsg f1 f2 "fatigue improved from sleeping" "it is more tired",
    valChgdMsg (fromEnum h1) (fromEnum h2) "health is worse!" "health is improved"
  ]
  where statusMsg Awake{} Sleeping{} = "Went sleeping"
        statusMsg Sleeping{} Awake{} = "Woke up"
        statusMsg _ _ = ""

valChgdMsg :: Ord a => a -> a -> String -> String -> String
valChgdMsg v1 v2 biggerV1 biggerV2
  | v1 == v2 = ""
  | v1 > v2 = biggerV1
  | otherwise = biggerV2
