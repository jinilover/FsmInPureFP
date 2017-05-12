{-# LANGUAGE OverloadedStrings #-}

module Fsm.ConstantLoaders where

import Data.Configurator
import Data.Configurator.Types
import Data.Semigroup
import Fsm.Constants.Types

loadConfig :: (Config -> IO a) -> String -> IO a
loadConfig f fileName = load [Required fileName] >>= f

loadValue :: Configured a => Name -> Name -> Config -> IO a
loadValue constName grpName cfg = require cfg ("Fsm.Constants." <> grpName <> constName)

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
