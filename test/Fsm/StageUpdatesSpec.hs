module Fsm.StageUpdatesSpec where

import Control.Monad (void)
import Data.List
import Data.Time
import Data.Semigroup
import Fsm.Stages
import Fsm.Constants
import Fsm.Utils
import Fsm.Commons
import Fsm.TestUtils
import Test.Hspec

-- Test the StageUpdate functions by applying them to the corresponding Stage

commonCheckDigest :: CheckUpdate
commonCheckDigest
  (Chicken _ l1 w1 _ (PooAmount t1 v1) _ _ _ _)
  (Chicken _ l2 w2 SoSo{} (PooAmount t2 v2) _ Awake{} _ Hardy)
  petConsts ct = do
                   let [lenFull, _, wtFull, _] = doubleConsts petConsts
                   l2 `shouldBe` l1 + Length lenFull
                   w2 `shouldBe` w1 + Weight wtFull
                   v2 `shouldBe` v1 + pooFromFull petConsts
commonCheckDigest _ _ _ _ = failDigest

commonCheckGrow :: Stage -> UTCTime -> Stage -> UTCTime -> Expectation
commonCheckGrow pet1 t1 pet2 t2 =
  do
    shouldEqual bornTime
    shouldEqual petLength
    shouldEqual weight
    shouldEqual $ poo . pooAmt
    shouldEqual $ moodValue . mood
    shouldEqual status
    shouldEqual $ fatigueValue . fatigue
    (\f -> (f pet1, f pet2)) <$> [fullnessTime . fullness, pooTime . pooAmt, moodTime . mood, fatigueTime . fatigue]
      `shouldSatisfy` all (== (t1, t2))
  where shouldEqual f = (length . nub) (f <$> [pet1, pet2]) `shouldBe` 1

-- a helper used by multiple tests in chickUpdateSpec
-- according Stages.hs, chickenAutoUpdates, i.e. [StageUpdate] functions, are applied on a pet when its stage,
-- after applying [StageUpdate] on the stage, only checking the final stage is sufficient,
-- this helper check the interim after each StageUpdate to make sure each StageUpdate is correct.
runAndCheck :: [CheckUpdate] -> Stage -> StageConstants -> UTCTime -> IO Stage
runAndCheck checkings orig consts ct =
  do
    result1 <- foldl check (return orig) $ zip chickenAutoUpdates checkings
    let result2 = autoUpdate orig ct consts
    result1 `shouldBe` result2
    return result1
  where check acc (update, checking) = do
          old <- acc
          let new = update old ct consts
          checking old new consts ct
          return new

failDigest = expectationFailure "fails in checkDigest"

failConstipate = expectationFailure "fails in checkConstipate"

failDecreaseMood = expectationFailure "fails in checkDecreaseMood"

failDepress = expectationFailure "fails in checkDepress"

failRaiseFatigue = expectationFailure "fails in checkRaiseFatigue"

failFatigue = expectationFailure "fails in checkFatigue"

-- checkings to be used by 1st test inside chickUpdateSpec
checkDigest1 :: CheckUpdate
checkDigest1
  (Chicken _ l1 w1 _ (PooAmount t1 v1) _ _ _ _)
  (Chicken _ l2 w2 Hungry{} (PooAmount t2 v2) _ Awake{} _ Healthy)
  petConsts ct = do
                   let [_, _, _, _, pSoSo, _digestSecs, _, _] = intConsts petConsts
                   l2 `shouldBe` l1
                   w2 `shouldBe` w1 - (Weight . weightLoss) petConsts
                   v2 `shouldBe` v1 + pSoSo
                   t2 `shouldSatisfy` (> t1)
                   t2 `shouldSatisfy` \t2 -> secsSpent t2 ct < _digestSecs
checkDigest1 _ _ _ _ = failDigest

checkConstipate1 :: CheckUpdate
checkConstipate1 (Chicken _ _ _ _ p1 _ _ _ _) (Chicken _ _ _ _ p2 _ Awake{} _ Healthy)  _ _ =
  p1 `shouldBe` p2
checkConstipate1  _ _ _ _ = failConstipate

checkDecreaseMood1 :: CheckUpdate
checkDecreaseMood1 (Chicken _ _ _ _ _ (Mood t1 v1) _ _ _) (Chicken _ _ _ _ _ (Mood t2 v2) Awake{} _ Healthy) _ _ =
  (v2 `shouldBe` v1 - 1) >> (t2 `shouldSatisfy` (> t1))
checkDecreaseMood1 _ _ _ _ = failDecreaseMood

checkDepress1 :: CheckUpdate
checkDepress1 (Chicken _ _ _ _ _ m1 _ _ _) (Chicken _ _ _ _ _ m2 Awake{} _ Healthy) _ _ =
  m1 `shouldBe` m2
checkDepress1 _ _ _ _ = failDepress

checkRaiseFatigue1 :: CheckUpdate
checkRaiseFatigue1 (Chicken _ _ _ _ _ _ _ (Fatigue t1 v1) _) (Chicken _ _ _ _ _ _ Awake{} (Fatigue t2 v2) Healthy) _ _ =
  (v2 `shouldBe` v1 + 1) >> (t2 `shouldSatisfy` (> t1))
checkRaiseFatigue1 _ _ _ _ = failRaiseFatigue

checkFatigue1 :: CheckUpdate
checkFatigue1 (Chicken _ _ _ _ _ _ _ f1 _) (Chicken _ _ _ _ _ _ Awake{} f2 Healthy) _ _ =
  f2 `shouldBe` f1
checkFatigue1 _ _ _ _ = failFatigue

checkSingleConstipate1 :: Stage -> Stage -> Expectation
checkSingleConstipate1 (Chicken _ _ _ _ (PooAmount t1 v1) _ _ _ _) (Chicken _ _ _ _ (PooAmount t2 v2) _ Awake{} _ Fair) =
  (v2 `shouldBe` v1) >> (t2 `shouldSatisfy` (> t1))
checkSingleConstipate1 _ _ = failConstipate

-- checkings to be used by 2nd test inside chickUpdateSpec
checkDigest2 :: CheckUpdate
checkDigest2
  c1@(Chicken _ _ _ _ (PooAmount t1 _) _ _ _ _)
  c2@(Chicken _ _ _ _ (PooAmount t2 _) _ _ _ _)
  petConsts ct =
    do
      commonCheckDigest c1 c2 petConsts ct
      t2 `shouldBe` t1
      t2 `shouldSatisfy` \t2 -> secsSpent t2 ct >= digestSecs petConsts

checkConstipate2 :: CheckUpdate
checkConstipate2 (Chicken _ _ _ _ (PooAmount t1 v1) _ _ _ _) (Chicken _ _ _ _ (PooAmount t2 v2) _ Awake{} _ Healthy)  _ _ =
  (v2 `shouldBe` v1) >> (t2 `shouldSatisfy` (> t1))
checkConstipate2  _ _ _ _ = failConstipate

checkDecreaseMood2 = checkDecreaseMood1

checkDepress2 = checkDepress1

checkRaiseFatigue2 = checkRaiseFatigue1

checkFatigue2 = checkFatigue1

-- checkings to be used by 3rd test inside chickUpdateSpec
checkDigest3 :: CheckUpdate
checkDigest3
  c1@(Chicken _ _ _ _ (PooAmount t1 _) _ _ _ _)
  c2@(Chicken _ _ _ _ (PooAmount t2 _) _ _ _ _)
  petConsts ct =
    do
      commonCheckDigest c1 c2 petConsts ct
      t2 `shouldSatisfy` (> t1)
      t2 `shouldSatisfy` \t2 -> secsSpent t2 ct < digestSecs petConsts

checkConstipate3 :: CheckUpdate
checkConstipate3 (Chicken _ _ _ _ p1 _ _ _ _) (Chicken _ _ _ _ p2 _ Awake{} _ Hardy)  _ _ =
  p1 `shouldBe` p2
checkConstipate3  _ _ _ _ = failConstipate

checkDecreaseMood3 :: CheckUpdate
checkDecreaseMood3 (Chicken _ _ _ _ _ (Mood t1 v1) _ _ _) (Chicken _ _ _ _ _ (Mood t2 v2) Awake{} _ Hardy) _ _ =
  (v2 `shouldBe` v1 - 1) >> (t2 `shouldBe` t1)
checkDecreaseMood3 _ _ _ _ = failDecreaseMood

checkDepress3 :: CheckUpdate
checkDepress3 s1 s2 _ _ = checkSingleDepress4 s1 s2

checkRaiseFatigue3 = checkRaiseFatigue1

checkFatigue3 = checkFatigue1

-- checkings to be used by 4th test inside chickUpdateSpec
checkDigest4 = checkDigest3

checkConstipate4 = checkConstipate3

checkDecreaseMood4 :: CheckUpdate
checkDecreaseMood4 (Chicken _ _ _ _ _ (Mood t1 v1) _ _ _) (Chicken _ _ _ _ _ (Mood t2 v2) Awake{} _ Hardy) _ _ =
  (v2 `shouldBe` v1 - 1) >> (t2 `shouldSatisfy` (> t1))
checkDecreaseMood4 _ _ _ _ = failDecreaseMood

checkDepress4 :: CheckUpdate
checkDepress4 (Chicken _ _ _ _ _ m1 _ _ _) (Chicken _ _ _ _ _ m2 Awake{} _ Hardy) _ _ =
  m1 `shouldBe` m2
checkDepress4 _ _ _ _ = failDepress

checkRaiseFatigue4 :: CheckUpdate
checkRaiseFatigue4 (Chicken _ _ _ _ _ _ _ (Fatigue t1 v1) _) (Chicken _ _ _ _ _ _ Awake{} (Fatigue t2 v2) Hardy) _ _ =
  (v2 `shouldBe` v1 + 1) >> (t2 `shouldSatisfy` (> t1))
checkRaiseFatigue4 _ _ _ _ = failRaiseFatigue

checkFatigue4 :: CheckUpdate
checkFatigue4 (Chicken _ _ _ _ _ _ _ f1 _) (Chicken _ _ _ _ _ _ Awake{} f2 Hardy) _ _ =
  f2 `shouldBe` f1
checkFatigue4 _ _ _ _ = failFatigue

checkSingleDepress4 :: Stage -> Stage -> Expectation
checkSingleDepress4 (Chicken _ _ _ _ _ (Mood t1 v1) _ _ _) (Chicken _ _ _ _ _ (Mood t2 v2) Awake{} _ Healthy) =
  (v2 `shouldBe` v1) >> (t2 `shouldSatisfy` (> t1))
checkSingleDepress4 _ _ = failDepress

-- checkings to be used by 5th test inside chickUpdateSpec
checkDigest5 = checkDigest3

checkConstipate5 = checkConstipate3

checkDecreaseMood5 :: CheckUpdate
checkDecreaseMood5 (Chicken _ _ _ _ _ (Mood t1 _) _ _ _) (Chicken _ _ _ _ _ (Mood t2 v2) Awake{} _ Hardy) petConsts _ =
  (v2 `shouldBe` depressIndex petConsts) >> (t2 `shouldSatisfy` (> t1))
checkDecreaseMood5 _ _ _ _ = failDecreaseMood

checkDepress5 = checkDepress4

checkRaiseFatigue5 :: CheckUpdate
checkRaiseFatigue5 (Chicken _ _ _ _ _ _ _ (Fatigue t1 v1) _) (Chicken _ _ _ _ _ _ Awake{} (Fatigue t2 v2) Hardy) _ _ =
  (v2 `shouldBe` v1 + 1) >> (t2 `shouldBe` t1)
checkRaiseFatigue5 _ _ _ _ = failRaiseFatigue

checkFatigue5 :: CheckUpdate
checkFatigue5 s1 s2 _ _ = checkSingleFatigue6 s1 s2

-- checkings to be used by 6th test inside chickUpdateSpec
checkDigest6 = checkDigest3

checkConstipate6 = checkConstipate3

checkDecreaseMood6 = checkDecreaseMood4

checkDepress6 = checkDepress4

checkRaiseFatigue6 = checkRaiseFatigue4

checkFatigue6 = checkFatigue4

checkSingleFatigue6 :: Stage -> Stage -> Expectation
checkSingleFatigue6 (Chicken _ _ _ _ _ _ _ (Fatigue t1 v1) _) (Chicken _ _ _ _ _ _ Awake{} (Fatigue t2 v2) Healthy) =
  (v2 `shouldBe` v1) >> (t2 `shouldSatisfy` (> t1))
checkSingleFatigue6 _ _ = failFatigue

-- checkings to be used by 7th test inside chickUpdateSpec
compareStates7 :: Stage -> Stage -> Expectation
compareStates7 (Chicken t1 l1 w1 fullness1 p1 m1 (Awake st1) (Fatigue fT1 fV1) h1)
  (Chicken t2 l2 w2 fullness2 p2 m2 (Sleeping st2) (Fatigue fT2 fV2) h2) =
    do
      t2 `shouldBe` t1
      l2 `shouldBe` l1
      w2 `shouldBe` w1
      fullness2 `shouldBe` fullness1
      p2 `shouldBe` p1
      m2 `shouldBe` m1
      st2 `shouldSatisfy` (> st1)
      fV2 `shouldBe` fV1 - 1
      fT2 `shouldSatisfy` (> fT1)
      h2 `shouldBe` h1
compareStates7 _ _ = expectationFailure "Fails in compareStates7"

-- checkings to be used by 8th test inside chickUpdateSpec
checkImmediateResult8 :: Stage -> Stage -> StageConstants -> Expectation
checkImmediateResult8 (Chicken _ l1 w1 _ _ _ _ f1 _)
  (Chicken _ l2 w2 SoSo{} (PooAmount _ pV2) (Mood _ 1) Awake{} f2 Hardy)
  petConsts =
    do
      let [lenFull,lenPlay, _, _] = doubleConsts petConsts
      l2 `shouldBe` l1 + Length (lenFull + lenPlay)
      w2 `shouldBe` w1
      pV2 `shouldBe` pooFromFull petConsts
      f2 `shouldBe` f1
checkImmediateResult8 _ _ _ = expectationFailure "Fails in checkImmediateResult8"

checkWaitingResult8 :: Stage -> Stage -> StageConstants -> Expectation
checkWaitingResult8 (Chicken _ l1 w1 _ _ _ _ (Fatigue fT1 fV1) _)
  (Chicken _ l2 w2 Hungry{} (PooAmount _ pV2) (Mood _ 0) Awake{} (Fatigue fT2 fV2) Hardy)
  petConsts =
    do
      let [lenFull, lenPlay, wtFull, wtLoss] = doubleConsts petConsts
          [_, _, _, pFull, pSoSo, _, _, _]= intConsts petConsts
      pV2 `shouldBe` pFull + pSoSo
      l2 `shouldBe` l1 + Length (lenFull + lenPlay)
      w2 `shouldBe` w1 + Weight (wtFull - wtLoss)
      fT2 `shouldSatisfy` (> fT1)
      fV2 `shouldBe` fV1 + 1
checkWaitingResult8 _ _ _ = expectationFailure "Fails in checkWaitingResult8"

-- checkings to be used by 10th test inside chickUpdateSpec
checkSingleFeed10 :: Stage -> Stage -> UTCTime -> Expectation
checkSingleFeed10 (Chicken _ _ _ (Hungry fT1) _ _ _ _ _)
  (Chicken _ _ _ (SoSo fT2) _ _ _ _ _) ct =
  [fT1, fT2] `shouldSatisfy` all (== ct)
checkSingleFeed10 _ _ _ = expectationFailure "Fails in checkSingleFeed10"

-- checkings to be used by 11th test inside chickUpdateSpec
checkGrow11 :: Stage -> UTCTime -> Stage -> UTCTime -> Expectation
checkGrow11 pet1@(Chicken _ _ _ Hungry{} PooAmount{} Mood{} _ Fatigue{} Healthy) t1
  pet2@(Adult _ _ _ Hungry{} PooAmount{} Mood{} _ Fatigue{} Healthy) t2 =
    commonCheckGrow pet1 t1 pet2 t2
checkGrow11 _ _ _ _ = expectationFailure "Fails in checkGrow11"

-- checkings to be used by 12th test inside chickUpdateSpec
checkPoop12 :: Stage -> Stage -> Expectation
checkPoop12 (Chicken _ _ _ _ (PooAmount pT1 pV1) _ _ _ _)
  (Chicken _ _ _ _ (PooAmount pT2 0) _ _ _ _) =
  do
    pT2 `shouldSatisfy` (> pT1)
    pV1 `shouldSatisfy` (> 0)
checkPoop12 _ _ = expectationFailure "Fails in checkPoop12"

-- Test the StageUpdate functions under different conditions on Stage Chicken
chickUpdateSpec :: AllConstants -> Spec
chickUpdateSpec allConsts =
  describe "test updates on Chicken under different condition" $ do
    it "1 executes chickenAutoUpdates, detects change by constipateEffect after another waiting" $ do
      c1 <- createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      -- the order of the checking item is important, the note in runAndCheck gives explanation
      let checkings = [checkDigest1, checkConstipate1, checkDecreaseMood1, checkDepress1, checkRaiseFatigue1, checkFatigue1]
      c2 <- runAndCheck checkings c1 petConsts t2
      t3 <- timeAfter _digestSecs
      checkSingleConstipate1 c2 $ constipateEffect c2 t3 petConsts
    it "2 executes chickenAutoUpdates, detects change by constipateEffect within chickenAutoUpdates" $ do
      let chickForTest c@(Chicken _ _ _ f p _ _ _ _) =
            c {fullness = Full $ fullnessTime f, pooAmt = p {poo = _pooLimit}}
      c1 <- chickForTest . createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      let checkings = [checkDigest2, checkConstipate2, checkDecreaseMood2, checkDepress2, checkRaiseFatigue2, checkFatigue2]
      void $ runAndCheck checkings c1 petConsts t2
    it "3 executes chickenAutoUpdates, detects change by depressEffect within chickenAutoUpdates" $ do
      let chickForTest c@(Chicken _ _ _ _fullness _ m _ f _) =
            c {fullness = Full $ fullnessTime _fullness,
               mood = m {moodValue = _depressIndex},
               fatigue = f {fatigueValue = _fatigueLimit - 1}}
      c1 <- chickForTest . createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      let checkings = [checkDigest3, checkConstipate3, checkDecreaseMood3, checkDepress3, checkRaiseFatigue3, checkFatigue3]
      void $ runAndCheck checkings c1 petConsts t2
    it "4 executes chickenAutoUpdates, set mood index just above depressIndex to check health, check health again after another waiting" $ do
      let chickForTest c@(Chicken _ _ _ _fullness p m _ _ _) =
            c {fullness = Full $ fullnessTime _fullness,
               pooAmt = p {poo = 0},
               mood = m {moodValue = _depressIndex + 1},
               health = Hardy}
      c1 <- chickForTest . createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      let checkings = [checkDigest4, checkConstipate4, checkDecreaseMood4, checkDepress4, checkRaiseFatigue4, checkFatigue4]
      c2 <- runAndCheck checkings c1 petConsts t2
      t3 <- timeAfter _digestSecs
      checkSingleDepress4 c2 $ depressEffect c2 t3 petConsts
    it "5 executes chickenAutoUpdates, detects change by fatigueEffect within chickenAutoUpdates" $ do
      let chickForTest c@(Chicken _ _ _ _fullness _ m _ f _) =
            c {fullness = Full $ fullnessTime _fullness,
               mood = m {moodValue = _depressIndex + 1},
               fatigue = f {fatigueValue = _fatigueLimit}}
      c1 <- chickForTest . createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      let checkings = [checkDigest5, checkConstipate5, checkDecreaseMood5, checkDepress5, checkRaiseFatigue5, checkFatigue5]
      void $ runAndCheck checkings c1 petConsts t2
    it "6 executes chickenAutoUpdates, set fatigue just before limit to check health, check health again after another waiting" $ do
      let chickForTest c@(Chicken _ _ _ _fullness _ _ _ f _) =
            c {fullness = Full $ fullnessTime _fullness,
               fatigue = f {fatigueValue = _fatigueLimit - 1},
               health = Hardy}
      c1 <- chickForTest . createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      let checkings = [checkDigest6, checkConstipate6, checkDecreaseMood6, checkDepress6, checkRaiseFatigue6, checkFatigue6]
      c2 <- runAndCheck checkings c1 petConsts t2
      t3 <- timeAfter _digestSecs
      checkSingleFatigue6 c2 $ fatigueEffect c2 t3 petConsts
    it "7 executes chickenAutoUpdates & sleep or sleepIfTired, to check its statuses after different conditions" $ do
      let chickForTest c@(Chicken _ _ _ _ _ _ _ f _) = c {fatigue = f {fatigueValue = _fatigueLimit - 1}}
          [withSleep, withSleepIfTired] = compose autoUpdate <$> [sleep, sleepIfTired]
      t1 <- getCurrentTime
      let c1 = chickForTest $ createChicken t1
      -- test when fatigue is 0, check if it can be forced to sleep
      let energeticChick = c1 {fatigue = Fatigue t1 0}
      let energeticChickToSleep = withSleep energeticChick t1 petConsts
      status energeticChickToSleep `shouldSatisfy` isSleeping
      fatigue energeticChickToSleep `shouldBe` fatigue energeticChick
      -- test the sleep function, ensure it's forced to sleep even not reach fatigue limit
      t2 <- timeAfter _maxAwakeSecs
      let awakeChick = autoUpdate c1 t2 petConsts
      t3 <- timeAfter 1 -- aims to provide different t2/t3 to simulate a realistic condition
      compareStates7 awakeChick $ withSleep awakeChick t3 petConsts
      -- test the sleepIfTired function, ensure it's sleeping only if it reach fatigue limit
      let awake = status $ withSleepIfTired c1 t3 petConsts
      awake `shouldNotSatisfy` isSleeping
      let sleeping = status $ withSleepIfTired c1 {
                       fatigue = (fatigue c1) {fatigueValue = _fatigueLimit}} t3 petConsts
      sleeping `shouldSatisfy` isSleeping
      -- test and check its states when its fatigue exceeds the limit
      t4 <- timeAfter 1
      let exceedLimitChick = c1 {fatigue = Fatigue t4 $ _fatigueLimit + 1}
      t5 <- timeAfter 1
      let exceedLimitChickToSleep = withSleep exceedLimitChick t5 petConsts
      status exceedLimitChickToSleep `shouldSatisfy` isSleeping
      (fatigueValue . fatigue) exceedLimitChickToSleep `shouldBe` _fatigueLimit
      (fatigueTime . fatigue) exceedLimitChickToSleep `shouldBe` (fatigueTime . fatigue) exceedLimitChick
      -- test and check its states when its fatigue just reach the limit
      -- result should be different from previous test although it goes to sleep as well
      let justLimitChick = c1 {fatigue = Fatigue t4 _fatigueLimit}
      let justLimitChickToSleep = withSleep justLimitChick t5 petConsts
      status justLimitChickToSleep `shouldSatisfy` isSleeping
      (fatigueValue . fatigue) justLimitChickToSleep `shouldBe` _fatigueLimit - 1
      (fatigueTime . fatigue) justLimitChickToSleep `shouldSatisfy` (> (fatigueTime . fatigue) justLimitChick)
    it "8 executes chickenAutoUpdates & play" $ do
      let chickForTest c@(Chicken _ _ _ _fullness p _ _ f _) =
            c {fullness = Full $ fullnessTime _fullness,
               pooAmt = p {poo = 0},
               fatigue = f {fatigueValue = _fatigueLimit}}
          withPlay = compose autoUpdate playEffect
      t1 <- getCurrentTime
      let c1 = chickForTest $ createChicken t1
      -- test withPlay immediately
      checkImmediateResult8 c1 (withPlay c1 t1 petConsts) petConsts
      -- test withPlay after waiting
      t2 <- timeAfter _digestSecs
      checkWaitingResult8 c1 (withPlay c1 t2 petConsts) petConsts
    it "9 executes chickenAutoUpdates & medicate" $ do
      let chickForTest c@(Chicken _ _ _ _fullness _ m _ f _) =
            c {fullness = Hungry $ fullnessTime _fullness,
               mood = m {moodValue = _depressIndex},
               fatigue = f {fatigueValue = _fatigueLimit}}
          withMedicate = compose autoUpdate medicate
      c1 <- (chickForTest . createChicken) <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      -- Hungry, depress, fatigue limit makes Healthy downgraded to Sick, medicate will be applied
      health (withMedicate c1 t2 petConsts) `shouldBe` Healthy
      -- Hungry, depress, fatigue limit makes Hardy downgraded to Weak only, medicate won't be applied
      health (withMedicate c1 {health = Hardy} t2 petConsts) `shouldBe` Weak
    it "10 executes chickenAutoUpdates & feed" $ do
      c1 <- createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      -- feed once
      checkSingleFeed10 (autoUpdate c1 t2 petConsts) (compose autoUpdate feed c1 t2 petConsts) t2
      -- feed twice
      fullness (composeFuncs [autoUpdate, feed, feed] c1 t2 petConsts) `shouldBe` Full t2
    it "11 executes chickenAutoUpdates & grow to adult" $ do
      let withGrow = compose autoUpdate grow
      c1 <- createChicken <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      -- wait more time to grow
      t3 <- timeAfter $ _duration - _digestSecs + 2
      checkGrow11 (withGrow c1 t2 petConsts) t2 (withGrow c1 t3 petConsts) t3
    it "12 executes chickenAutoUpdates & poop" $ do
      let chickForTest c@(Chicken _ _ _ _ p _ _ _ _) =
            c {pooAmt = p {poo = _pooLimit - _pooFromSoSo}}
          withPoop = compose autoUpdate poop
      t1 <- getCurrentTime
      let c1 = chickForTest $ createChicken t1
      -- no digest yet, poo still < pooLimit, poop no applied
      pooAmt (withPoop c1 t1 petConsts) `shouldBe` pooAmt c1
      -- digest happens, poop applied, poo is 0
      t2 <- timeAfter _digestSecs
      checkPoop12 c1 $ withPoop c1 t2 petConsts
  where petConsts = chickenConsts allConsts
        [_duration, _maxAwakeSecs, _pooLimit, _, _pooFromSoSo, _digestSecs, _depressIndex, _fatigueLimit] =
          intConsts petConsts
        createChicken ct = (basePet ct allConsts Chicken) { pooAmt = PooAmount ct $ _pooLimit - 1,
                                                            health = Healthy }

checkAdultGrow :: Stage -> UTCTime -> Stage -> UTCTime -> StageConstants -> Expectation
checkAdultGrow adult@(Adult _ _ _ Hungry{} _ _ _ _ Healthy) t1
  elder@(Elder _ _ _ Hungry{} _ _ _ _ Healthy t n) t2 adultConsts = do
    commonCheckGrow adult t1 elder t2
    t `shouldBe` t2
    n `shouldBe` medAllowsForElder adultConsts
checkAdultGrow _ _ _ _ _ = expectationFailure "Fails in checkAdultGrow"

-- Test the StageUpdate function specific to Stage Adult
adultUpdateSpec :: AllConstants -> Spec
adultUpdateSpec allConsts =
  describe "test updates on Adult specific to Adult" $
    it "executes adultAutoUpdates & grow" $ do
      let [_duration, _, _pooLimit, _, _, _digestSecs, _, _] = intConsts _adultConsts
          adultForTest ct = (basePet ct allConsts Adult) { pooAmt = PooAmount ct $ _pooLimit - 1,
                                                           health = Healthy}
          withGrow = compose autoUpdate grow
      adult <- adultForTest <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      t3 <- timeAfter $ _duration - _digestSecs
      let [pet1, pet2] = (\t -> withGrow adult t _adultConsts) <$> [t2, t3]
      checkAdultGrow pet1 t2 pet2 t3 _adultConsts
  where _adultConsts = adultConsts allConsts

-- Test the StageUpdate function specific to Stage Elder
elderUpdateSpec :: AllConstants -> Spec
elderUpdateSpec allConsts =
  describe "test updates on Elder specific to Elder" $
    it "executes elderAutoUpdates to test the weakerInSecs" $ do
      let elderForTest ct = (basePet ct allConsts Elder ct $ medAllowsForElder _adultConsts) {
                              fullness = Full ct
                            }
      elder <- elderForTest <$> getCurrentTime
      t2 <- timeAfter _digestSecs
      t3 <- timeAfter $ _weakerInSecs - _digestSecs
      -- in test1, health upgraded to Healthy due to digest from full,
      -- in test2, wait to reach weakerInSecs, this time healthy downgraded to Fair
      (\t -> health $ autoUpdate elder t _elderConsts) <$> [t2, t3] `shouldBe` [Healthy, Fair]
  where [_adultConsts, _elderConsts] = [adultConsts, elderConsts] <*> [allConsts]
        [_digestSecs, _weakerInSecs] = [digestSecs, weakerInSecs] <*> [_elderConsts]

intConsts :: StageConstants -> [Int]
intConsts petConsts =
  [duration, maxAwakeSecs, pooLimit, pooFromFull, pooFromSoSo, digestSecs, depressIndex, fatigueLimit] <*> [petConsts]

doubleConsts :: StageConstants -> [Double]
doubleConsts petConsts =
  [lengthFromFull, lengthFromPlay, weightFromFull, weightLoss] <*> [petConsts]

spec :: AllConstants -> Expectation
spec allConsts =
  foldl (\io s -> io >> hspec (s allConsts)) (return ()) [chickUpdateSpec, adultUpdateSpec, elderUpdateSpec]
