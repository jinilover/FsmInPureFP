module Fsm.TestUtils where

import Data.Time
import Fsm.Commons
import Fsm.Constants
import Fsm.Stages
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
  where [l, w] = [baseLength, baseWeight] <*> [eggConsts allConsts]

baseEgg :: UTCTime -> AllConstants -> Stage
baseEgg ct allConsts = Egg ct temp (temp * _duration) Healthy
  where [temp, _duration] = [medTemp, duration] <*> [eggConsts allConsts]
