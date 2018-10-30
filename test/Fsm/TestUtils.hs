module Fsm.TestUtils where

import Data.Time
import Fsm.Commons
import Fsm.Commons.Types
import Fsm.ConstantLoaders
import Fsm.Constants.Types
import Fsm.Stages.Types
import Test.Hspec

type CheckUpdate = Stage -> Stage -> StageConstants -> UTCTime -> Expectation

type CommonPetStruct a = UTCTime ->
                         Length ->
                         Weight ->
                         Fullness ->
                         PooAmount ->
                         Mood ->
                         Status ->
                         Fatigue ->
                         Health ->
                         a

basePet :: UTCTime -> AllConstants -> CommonPetStruct a -> a
basePet ct allConsts f =
  f ct (Length l) (Weight w) (SoSo ct) (PooAmount ct 0) (Mood ct 0) (Awake ct) (Fatigue ct 0) Fair
  where [l, w] = ($ eggConsts allConsts) <$> [baseLength, baseWeight]

baseEgg :: UTCTime -> AllConstants -> Stage
baseEgg ct allConsts = Egg ct temp (temp * _duration) Healthy
  where [temp, _duration] = ($ eggConsts allConsts) <$> [medTemp, duration]
