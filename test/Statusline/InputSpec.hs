module Statusline.InputSpec (spec) where

import Statusline.Input
import Test.Hspec

spec :: Spec
spec = describe "parseInput" $ do
  context "normal" $ do
    it "extracts every field" $
      parseInput
        "{\"workspace\":{\"current_dir\":\"/repo\"},\"transcript_path\":\"/t.jsonl\",\
        \\"model\":{\"display_name\":\"Opus\"},\"effort\":{\"level\":\"high\"},\
        \\"rate_limits\":{\"five_hour\":{\"used_percentage\":10,\"resets_at\":1735723800},\
        \\"seven_day\":{\"used_percentage\":5}},\"context_window\":{\"used_percentage\":30}}"
        `shouldBe` StatusInput
          { siCwd = Just "/repo"
          , siContextPct = Just 30
          , siFiveHour = Just 10
          , siSevenDay = Just 5
          , siResetsAt = Just 1735723800
          , siTranscript = Just "/t.jsonl"
          , siModel = Just "Opus"
          , siEffort = Just "high"
          }
    it "falls back from workspace.current_dir to cwd" $
      siCwd (parseInput "{\"cwd\":\"/fallback\"}") `shouldBe` Just "/fallback"
    it "falls back from model.display_name to model.id" $
      siModel (parseInput "{\"model\":{\"id\":\"claude-opus-4-8\"}}")
        `shouldBe` Just "claude-opus-4-8"
    it "extracts effort.level" $
      siEffort (parseInput "{\"effort\":{\"level\":\"xhigh\"}}") `shouldBe` Just "xhigh"

  context "boundary" $ do
    it "empty object -> all absent" $ parseInput "{}" `shouldBe` emptyInput
    it "null fields -> absent" $
      parseInput "{\"workspace\":{\"current_dir\":null},\"context_window\":null}"
        `shouldBe` emptyInput
    it "model present without effort -> effort absent" $ do
      let i = parseInput "{\"model\":{\"display_name\":\"Opus\"}}"
      siModel i `shouldBe` Just "Opus"
      siEffort i `shouldBe` Nothing
    it "no model -> model absent" $
      siModel (parseInput "{\"effort\":{\"level\":\"high\"}}") `shouldBe` Nothing

  context "abnormal" $ do
    it "malformed json -> emptyInput" $
      parseInput "not valid json at all" `shouldBe` emptyInput
    it "wrong field types -> absent, parse survives" $
      parseInput "{\"context_window\":{\"used_percentage\":\"lots\"},\"cwd\":42}"
        `shouldBe` emptyInput
    it "wrong-typed effort.level -> absent, parse survives" $
      siEffort (parseInput "{\"model\":{\"display_name\":\"Opus\"},\"effort\":{\"level\":7}}")
        `shouldBe` Nothing

  describe "validEpoch" $ do
    it "non-negative integer accepted" $ validEpoch 1735723800 `shouldBe` Just 1735723800
    it "zero accepted" $ validEpoch 0 `shouldBe` Just 0
    it "fractional rejected" $ validEpoch 1.5 `shouldBe` Nothing
    it "negative rejected" $ validEpoch (-1) `shouldBe` Nothing
