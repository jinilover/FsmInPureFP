module Fsm.Utils where

import Control.Concurrent
import Data.List
import Data.Semigroup
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf

secsSpent :: UTCTime -> UTCTime -> Int
secsSpent ownTime ct = (round . diffUTCTime ct) ownTime

sleepInSecs :: Int -> IO ()
sleepInSecs = threadDelay . (10^6 *)

timeAfter :: Int -> IO UTCTime
timeAfter secs = sleepInSecs secs >> getCurrentTime

downgrade :: Enum a => a -> a
downgrade enum
  | fromEnum enum == 0 = enum
  | otherwise = pred enum

roundup :: Int -> Double -> String
roundup decimals = printf $ "%." <> show decimals <> "f"

format :: UTCTime -> String
format = formatTime defaultTimeLocale "%T"

type COMPOSE_FT s t u = s -> t -> u -> s

unit s _ _ = s

compose :: COMPOSE_FT s t u -> COMPOSE_FT s t u -> COMPOSE_FT s t u
compose f1 f2 s t u = f2 (f1 s t u) t u

composeFuncs :: [COMPOSE_FT s t u] -> COMPOSE_FT s t u
composeFuncs = foldl compose unit

-- a simple func that simulate a random composition,
-- if current time converted to posix secons is divisible by 5, the 2nd
-- optional functions will be included, o.w. not
composeRandomFuncs :: UTCTime -> [COMPOSE_FT s t u] -> [COMPOSE_FT s t u] -> COMPOSE_FT s t u
composeRandomFuncs ct fs1
  | (round . utcTimeToPOSIXSeconds) ct `div` 5 == 0 = (const . composeFuncs) fs1
  | otherwise = foldl compose $ composeFuncs fs1

updateListByFuncs :: [a] -> [[a] -> [a]] -> [a]
updateListByFuncs = foldl $ \as f -> f as

listToString :: [String] -> String
listToString = intercalate "\n" . filter (/= "")
