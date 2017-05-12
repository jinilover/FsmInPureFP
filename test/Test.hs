-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import Data.Time
import Test.Hspec
import Fsm.ConstantLoaders
import Fsm.SimpleFsmSpec as SimpleFsmSpec
import Fsm.InputSpec as InputSpec
import Fsm.StageUpdatesSpec as StageUpdatesSpec

main :: IO ()
main = do
  allConsts <- loadAllConsts "test/resources/appl.cfg"
  foldl (\io s -> io >> s allConsts) (return ()) specs

specs = [SimpleFsmSpec.spec, InputSpec.spec, StageUpdatesSpec.spec]
