module Statusline.InputSpec (spec) where

import Statusline.Input
import Test.Hspec

spec :: Spec
spec = describe "parseInput" $ do
  context "normal" $ do
    it "extracts every field" $
      parseInput
        "{\"workspace\":{\"current_dir\":\"/repo\"},\"transcript_path\":\"/t.jsonl\",\
        \\"rate_limits\":{\"five_hour\":{\"used_percentage\":10,\"resets_at\":1735723800},\
        \\"seven_day\":{\"used_percentage\":5}},\"context_window\":{\"used_percentage\":30}}"
        `shouldBe` StatusInput
          { siCwd = Just "/repo"
          , siContextPct = Just 30
          , siFiveHour = Just 10
          , siSevenDay = Just 5
          , siResetsAt = Just 1735723800
          , siTranscript = Just "/t.jsonl"
          }
    it "falls back from workspace.current_dir to cwd" $
      siCwd (parseInput "{\"cwd\":\"/fallback\"}") `shouldBe` Just "/fallback"

  context "boundary" $ do
    it "empty object -> all absent" $ parseInput "{}" `shouldBe` emptyInput
    it "null fields -> absent" $
      parseInput "{\"workspace\":{\"current_dir\":null},\"context_window\":null}"
        `shouldBe` emptyInput

  context "abnormal" $ do
    it "malformed json -> emptyInput" $
      parseInput "not valid json at all" `shouldBe` emptyInput
    it "wrong field types -> absent, parse survives" $
      parseInput "{\"context_window\":{\"used_percentage\":\"lots\"},\"cwd\":42}"
        `shouldBe` emptyInput

  describe "validEpoch" $ do
    it "non-negative integer accepted" $ validEpoch 1735723800 `shouldBe` Just 1735723800
    it "zero accepted" $ validEpoch 0 `shouldBe` Just 0
    it "fractional rejected" $ validEpoch 1.5 `shouldBe` Nothing
    it "negative rejected" $ validEpoch (-1) `shouldBe` Nothing
