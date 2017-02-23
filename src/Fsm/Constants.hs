{-# LANGUAGE OverloadedStrings #-}

module Fsm.Constants where

import Data.Configurator
import Data.Configurator.Types
import Data.Semigroup

-- load all constants configured in appl.cfg

data InputConstants = InputConstants { inputTimeout :: Int }

data StageConstants = EggConstants { fatalMinTemp :: Int,
                                     medTemp :: Int,
                                     fatalMaxTemp :: Int,
                                     fatalTempSecs :: Int,
                                     duration :: Int,
                                     baseLength :: Double,
                                     baseWeight :: Double } |
                      ChickenConstants { happyIndex :: Int,
                                         depressIndex :: Int,
                                         decreaseMoodSecs :: Int,
                                         depressSecs :: Int,
                                         pooLimit :: Int,
                                         pooLimitSecs :: Int,
                                         fatigueLimit :: Int,
                                         fatigueSecs :: Int,
                                         maxAwakeSecs :: Int,
                                         raiseFatigueSecs :: Int,
                                         lengthFromFull :: Double,
                                         weightFromFull :: Double,
                                         weightLoss :: Double,
                                         pooFromFull :: Int,
                                         pooFromSoSo :: Int,
                                         digestSecs :: Int,
                                         lengthFromPlay :: Double,
                                         sleepingSecs :: Int,
                                         duration :: Int } |
                      AdultConstants { happyIndex :: Int,
                                       depressIndex :: Int,
                                       decreaseMoodSecs :: Int,
                                       depressSecs :: Int,
                                       pooLimit :: Int,
                                       pooLimitSecs :: Int,
                                       fatigueLimit :: Int,
                                       fatigueSecs :: Int,
                                       maxAwakeSecs :: Int,
                                       raiseFatigueSecs :: Int,
                                       lengthFromFull :: Double,
                                       weightFromFull :: Double,
                                       weightLoss :: Double,
                                       pooFromFull :: Int,
                                       pooFromSoSo :: Int,
                                       digestSecs :: Int,
                                       lengthFromPlay :: Double,
                                       sleepingSecs :: Int,
                                       duration :: Int,
                                       medAllowsForElder :: Int } |
                      ElderConstants { happyIndex :: Int,
                                       depressIndex :: Int,
                                       decreaseMoodSecs :: Int,
                                       depressSecs :: Int,
                                       pooLimit :: Int,
                                       pooLimitSecs :: Int,
                                       fatigueLimit :: Int,
                                       fatigueSecs :: Int,
                                       maxAwakeSecs :: Int,
                                       raiseFatigueSecs :: Int,
                                       lengthFromFull :: Double,
                                       weightFromFull :: Double,
                                       weightLoss :: Double,
                                       pooFromFull :: Int,
                                       pooFromSoSo :: Int,
                                       digestSecs :: Int,
                                       lengthFromPlay :: Double,
                                       sleepingSecs :: Int,
                                       weakerInSecs :: Int} deriving Show

data AllConstants = AllConstants {
                          inputConsts :: InputConstants,
                          eggConsts :: StageConstants,
                          chickenConsts :: StageConstants,
                          adultConsts :: StageConstants,
                          elderConsts :: StageConstants
                        }

constsRoot = "Fsm.Constants."

loadConfig :: (Config -> IO a) -> String -> IO a
loadConfig f fileName = load [Required fileName] >>= f

-- a handy type alias for the lambda type in subsequent function usage
type CommonPetConsts a = Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Int ->
                         Double ->
                         Double ->
                         Double ->
                         Int ->
                         Int ->
                         Int ->
                         Double ->
                         Int ->
                         a

loadValue :: Configured a => Name -> Name -> Config -> IO a
loadValue constName grpName cfg = require cfg (constsRoot <> grpName <> constName)

loadCommonPetConsts :: Name -> CommonPetConsts a -> Config -> IO a
loadCommonPetConsts stageName createPet cfg =
  createPet <$> valueOf ".happyIndex"
            <*> valueOf ".depressIndex"
            <*> valueOf ".decreaseMoodSecs"
            <*> valueOf ".depressSecs"
            <*> valueOf ".pooLimit"
            <*> valueOf ".pooLimitSecs"
            <*> valueOf ".fatigueLimit"
            <*> valueOf ".fatigueSecs"
            <*> valueOf ".maxAwakeSecs"
            <*> valueOf ".raiseFatigueSecs"
            <*> valueOf ".lengthFromFull"
            <*> valueOf ".weightFromFull"
            <*> valueOf ".weightLoss"
            <*> valueOf ".pooFromFull"
            <*> valueOf ".pooFromSoSo"
            <*> valueOf ".digestSecs"
            <*> valueOf ".lengthFromPlay"
            <*> valueOf ".sleepingSecs"
            where valueOf constName = loadValue constName stageName cfg

loadChickConsts :: CommonPetConsts (Int -> StageConstants) -> Config -> IO StageConstants
loadChickConsts createPet cfg =
  let stageName = "ChickenConstants" in
  loadCommonPetConsts stageName createPet cfg <*> loadValue ".duration" stageName cfg

loadAdultConsts :: CommonPetConsts (Int -> Int -> StageConstants) -> Config -> IO StageConstants
loadAdultConsts createPet cfg =
  let stageName = "AdultConstants"
      valueOf constName = loadValue constName stageName cfg in
  loadCommonPetConsts stageName createPet cfg <*> valueOf ".duration"
                                              <*> valueOf ".medAllowsForElder"

loadElderConsts :: CommonPetConsts (Int -> StageConstants) -> Config -> IO StageConstants
loadElderConsts createPet cfg =
  let stageName = "ElderConstants" in
      -- valueOf constName = loadValue constName stageName cfg in
  loadCommonPetConsts stageName createPet cfg <*> loadValue ".weakerInSecs" stageName cfg

loadEggConsts :: Config -> IO StageConstants
loadEggConsts cfg = EggConstants <$> valueOf ".fatalMinTemp"
                                 <*> valueOf ".medTemp"
                                 <*> valueOf ".fatalMaxTemp"
                                 <*> valueOf ".fatalTempSecs"
                                 <*> valueOf ".duration"
                                 <*> valueOf ".baseLength"
                                 <*> valueOf ".baseWeight"
  where valueOf constName = loadValue constName "EggConstants" cfg

loadInputConstants :: Config -> IO InputConstants
loadInputConstants cfg = InputConstants <$> loadValue ".inputTimeout" "InputConstants" cfg

loadAllConsts :: String -> IO AllConstants
loadAllConsts file =
  AllConstants <$> loadConfig loadInputConstants file
               <*> loadConfig loadEggConsts file
               <*> loadConfig (loadChickConsts ChickenConstants) file
               <*> loadConfig (loadAdultConsts AdultConstants) file
               <*> loadConfig (loadElderConsts ElderConstants) file
