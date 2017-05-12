module Main where

import System.Timeout
import Data.Semigroup
import Data.Time
import Data.Foldable (toList)
import Data.Sequence (fromList)
import Text.Read
import Fsm.SimpleFsm
import Fsm.ConstantLoaders
import Fsm.Constants.Types
import Fsm.Stages.Types
import Fsm.Commons.Types
import Fsm.Commons
import Fsm.Utils

main :: IO ()
main =
  do
    allConsts <- loadAllConsts "src/resources/appl.cfg"
    [_timeout, _temp, _energyToHatch] <- readConsts allConsts
    egg <- (\time -> Egg time _temp _energyToHatch Healthy) <$> getCurrentTime
    finalMsg <- transit egg (prompt _timeout) eggInputs "An egg is born!" allConsts
    putStrLn finalMsg
    where readConsts allConsts =
            let [timeout, temp, _duration] = [inputTimeout . inputConsts, medTemp . eggConsts, duration . eggConsts] <*> [allConsts] in
              return [timeout, temp, temp * _duration]

-- prompt the user input by providing the available inputs
-- and output msg about the current state of the pet
-- 1st arg is the timeout in secs
-- if user enter invalid input or no input after timeout
-- it will interpret as nothing entered
prompt :: Int -> UserPrompt
prompt inputTimeout inputs msg = do
  ct <- getCurrentTime
  putStrLn msg
  case inputs of [] -> putStrLn "Sleeping ... you cannot enter any input at the moment"
                 _ -> putStrLn "Please choose the following option" >> (putStrLn . presentInputs) inputs
  inpStr <- timeout inputTimeout getLine
  return (ct, inpStr >>= toInput)
  where toInput s = (readMaybe s :: Maybe Int) >>=
          \i -> if null inputs || i < 1 || i > length inputs then Nothing
                else Just $ inputs !! (i - 1)

presentInputs :: [Input] -> String
presentInputs =
  listToString . foldl (\strs inp -> strs ++ [show (length strs + 1) <> ") " <> show inp]) []
