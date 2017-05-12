module Fsm.SimpleFsmSpec where

import Data.Time
import Data.Semigroup
import Test.Hspec
import Fsm.TestUtils
import Fsm.Constants.Types
import Fsm.SimpleFsm
import Fsm.Commons.Types
import Fsm.Utils
import Fsm.Stages.Types

-- Test the transit functions
killByInputSpec :: UTCTime -> AllConstants -> Spec
killByInputSpec ct allConsts =
  describe "detects exceeding temp. limit will kill the egg" $ do
    it "detects further increase the temperature when the egg reach fatal max" $
      transitTestTemplate egg (\_ _ -> return (ct, Just IncreaseTemp)) allConsts >>=
        (`shouldBe` "The egg has reached the max temperature, you've cooked it")
    it "detects further decrease the temperature when the egg reach fatal min" $
      transitTestTemplate egg (\_ _ -> return (ct, Just DecreaseTemp)) allConsts >>=
        (`shouldBe` "The egg has reached the min temperature, you've frozen it")
  where egg = baseEgg ct allConsts

killByTimeSpec :: AllConstants -> Spec
killByTimeSpec allConsts =
  describe "detects poor health egg suffering from fatal temp. for long time" $ do
    it "detects the egg suffers max temperature for a long time" $ do
      egg <- createEgg
      transitTestTemplate egg {currTemp = maxTemp} prompt allConsts >>=
        (`shouldBe` "The egg is in poor health and suffered the max temperature for at least " <>
          show timeout <> " secs, failed to hatch")
    it "detects the egg suffers min temperature for a long time" $ do
      egg <- createEgg
      transitTestTemplate egg {currTemp = minTemp} prompt allConsts >>=
        (`shouldBe` "The egg is in poor health and suffered the min temperature for at least " <>
          show timeout <> " secs, failed to hatch")
    where createEgg = (\ct -> (baseEgg ct allConsts) {health = Sick}) <$> getCurrentTime
          [timeout, maxTemp, minTemp] = [fatalTempSecs, fatalMaxTemp, fatalMinTemp] <*> [eggConsts allConsts]
          prompt _ _ = do
                         (sleepInSecs . (1 +)) timeout
                         (,) <$> getCurrentTime <*> return Nothing

transitTestTemplate :: Stage -> UserPrompt -> AllConstants -> IO String
transitTestTemplate egg prompt = transit egg prompt [] ""

spec :: AllConstants -> IO ()
spec allConsts = do
  getCurrentTime >>= \ct -> hspec $ killByInputSpec ct allConsts
  hspec $ killByTimeSpec allConsts
