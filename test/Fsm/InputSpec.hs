module Fsm.InputSpec where

import Data.Time
import Data.Set (fromList, empty)
import Test.Hspec
import Fsm.ConstantLoaders
import Fsm.Constants.Types
import Fsm.Stages
import Fsm.Stages.Types
import Fsm.Commons
import Fsm.Commons.Types
import Fsm.TestUtils

-- Test the [Input] generated according to the Stage and its states

type TestInputs = UTCTime -> AllConstants -> Spec

chickenSpec :: TestInputs
chickenSpec ct allConsts =
  describe "Should determine the chicken inputs according to its state" $ do
    it "determines a chicken's inputs when it's awake" $
      fromList (inputsByState base {health = Sick}) `shouldBe` fromList [Feed, Medication, Bed]
    it "returns no inputs when it's sleeping" $
      fromList (inputsByState base {status = Sleeping ct} ) `shouldBe` empty
    it "determines a chicken's inputs when it's Full and Sick" $
      fromList (inputsByState base {fullness = Full ct, health = Sick}) `shouldBe` fromList [Medication, Bed]
    it "determines a chicken's inputs when it's Hungry and Sick" $
      fromList (inputsByState base {fullness = Hungry ct, health = Sick}) `shouldBe` fromList [Feed, Medication, Bed]
    it "determines a chicken's inputs when it's Full and Fair health" $
      fromList (inputsByState base {fullness = Full ct}) `shouldBe` fromList [Play, Bed]
    it "determines a chicken's inputs when it's SoSo and Fair health" $
      fromList (inputsByState base) `shouldBe` fromList [Feed, Play, Bed]
    it "determines a chicken's inputs when it's Hungry and Fair health" $
      fromList (inputsByState base {fullness = Hungry ct}) `shouldBe` fromList [Feed, Play, Bed]
  where base = basePet ct allConsts Chicken

adultSpec :: TestInputs
adultSpec ct allConsts =
  describe "Should determine the adult inputs according to its state" $ do
    it "determines an adult's inputs when it's Full and Sick" $
      fromList (inputsByState base {fullness = Full ct, health = Sick}) `shouldBe` fromList [Sing, Medication, Bed]
    it "determines an adult's inputs when it's SoSo and Sick" $
      fromList (inputsByState base {health = Sick}) `shouldBe` fromList [Feed, Sing, Medication, Bed]
    it "determines an adult's inputs when it's Hungry and Sick" $
      fromList (inputsByState base {fullness = Hungry ct, health = Sick}) `shouldBe` fromList [Feed, Sing, Medication, Bed]
    it "determines an adult's inputs when it's Full and Fair health" $
      fromList (inputsByState base {fullness = Full ct}) `shouldBe` fromList [Sing, Play, Bed]
    it "determines an adult's inputs when it's SoSo and Fair health" $
      fromList (inputsByState base) `shouldBe` fromList [Feed, Sing, Play, Bed]
    it "determines an adult's inputs when it's Hungry and Fair health" $
      fromList (inputsByState base {fullness = Hungry ct}) `shouldBe` fromList [Feed, Sing, Play, Bed]
  where base = basePet ct allConsts Adult

eggSpec :: TestInputs
eggSpec ct allConsts =
  describe "Should determine the egg inputs according to its state" $
    it "determines an egg inputs" $
      fromList (inputsByState $ baseEgg ct allConsts) `shouldBe` fromList eggInputs

elderSpec :: TestInputs
elderSpec ct allConsts =
  describe "Should determine the elder inputs according to its state" $ do
    it "determines an elder's inputs when it's awake" $
      fromList (inputsByState base {health = Sick}) `shouldBe` fromList [Feed, Medication, Bed]
    it "returns no inputs when it's sleeping" $
      fromList (inputsByState base {status = Sleeping ct} ) `shouldBe` empty
    it "determines an elder's inputs when it's Full and Sick" $
      fromList (inputsByState base {fullness = Full ct, health = Sick} ) `shouldBe` fromList [Medication, Bed]
    it "determines an elder's inputs when it's SoSo and Sick" $
      fromList (inputsByState base {health = Sick} ) `shouldBe` fromList [Feed, Medication, Bed]
    it "determines an elder's inputs when it's Hungry and Sick" $
      fromList (inputsByState base {fullness = Hungry ct, health = Sick} ) `shouldBe` fromList [Feed, Medication, Bed]
    it "determines an elder's inputs when it's Hungry, Sick, used up medications" $
      fromList (inputsByState base {fullness = Hungry ct, health = Sick, medAllows = 0} ) `shouldBe` fromList [Feed, Bed]
    it "determines an elder's inputs when it's Full and Fair health" $
      fromList (inputsByState base {fullness = Full ct} ) `shouldBe` fromList [Play, Bed]
    it "determines an elder's inputs when it's SoSo and Fair health" $
      fromList (inputsByState base) `shouldBe` fromList [Feed, Play, Bed]
    it "determines an elder's inputs when it's Hungry and Fair health" $
      fromList (inputsByState base) `shouldBe` fromList [Feed, Play, Bed]
  where base = basePet ct allConsts Elder ct $ (medAllowsForElder . adultConsts) allConsts

spec :: AllConstants -> IO ()
spec allConsts = do
  ct <- getCurrentTime
  foldl (\io s -> io >> hspec (s ct allConsts)) (return ()) [eggSpec, chickenSpec, adultSpec, elderSpec]
