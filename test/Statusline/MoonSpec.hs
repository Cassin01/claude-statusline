module Statusline.MoonSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Statusline.Moon
import Test.Hspec

-- 2000-01-06 18:14 UTC reference new moon and mean synodic month in seconds.
newMoon :: Integer
newMoon = 947182440

synodic :: Integer
synodic = 2551443

wellFormed :: Text -> Bool
wellFormed t = any (`T.isPrefixOf` t) phases && "%" `T.isSuffixOf` t
  where
    phases = ["\x1F311", "\x1F312", "\x1F313", "\x1F314", "\x1F315", "\x1F316", "\x1F317", "\x1F318"]

spec :: Spec
spec = describe "moonPhase" $ do
  it "reference new moon -> new, 0%" $
    moonPhase newMoon `shouldBe` "\x1F311 0%"
  it "half a synodic month later -> full, 100%" $
    moonPhase (newMoon + synodic `div` 2) `shouldBe` "\x1F315 100%"
  it "quarter -> first quarter, 50%" $
    moonPhase (newMoon + synodic `div` 4) `shouldBe` "\x1F313 50%"
  it "three quarters -> last quarter, 50%" $
    moonPhase (newMoon + 3 * synodic `div` 4) `shouldBe` "\x1F317 50%"
  it "a full cycle later is new again" $
    moonPhase (newMoon + synodic) `shouldBe` "\x1F311 0%"
  it "epoch before the reference still well-formed" $
    moonPhase 0 `shouldSatisfy` wellFormed
  it "far-future epoch still well-formed" $
    moonPhase 9999999999 `shouldSatisfy` wellFormed
  it "moonEmoji matches the emoji part of moonPhase" $ do
    moonEmoji newMoon `shouldBe` "\x1F311"
    moonEmoji (newMoon + synodic `div` 2) `shouldBe` "\x1F315"
    moonEmoji (newMoon + synodic `div` 4) `shouldBe` "\x1F313"
