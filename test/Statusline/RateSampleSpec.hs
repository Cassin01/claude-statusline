module Statusline.RateSampleSpec (spec) where

import Statusline.RateSample
import Test.Hspec

spec :: Spec
spec = do
  describe "parseSamples / renderSamples" $ do
    it "round-trips" $ do
      let samples = [Sample 100 40, Sample 160 42.5, Sample 220 43]
      parseSamples (renderSamples samples) `shouldBe` samples
    it "sorts by time" $
      parseSamples "200 50\n100 40" `shouldBe` [Sample 100 40, Sample 200 50]
    it "malformed lines skipped" $
      parseSamples "garbage\n1 2 3\n-5 40\n100 40\n" `shouldBe` [Sample 100 40]
    it "empty input -> []" $ parseSamples "" `shouldBe` []

  describe "updateSamples" $ do
    context "normal" $ do
      it "appends when the percentage changed" $
        updateSamples 10 41 [Sample 0 40] `shouldBe` [Sample 0 40, Sample 10 41]
      it "skips an unchanged percentage inside the heartbeat" $
        updateSamples 59 40 [Sample 0 40] `shouldBe` [Sample 0 40]
      it "re-records an unchanged percentage after the heartbeat" $
        updateSamples 60 40 [Sample 0 40] `shouldBe` [Sample 0 40, Sample 60 40]
      it "empty store gets the first sample" $
        updateSamples 5 40 [] `shouldBe` [Sample 5 40]
    context "boundary" $ do
      it "sample exactly at the window edge is kept" $
        updateSamples 2000 50 [Sample (2000 - windowSecs) 40]
          `shouldBe` [Sample (2000 - windowSecs) 40, Sample 2000 50]
      it "sample just past the window edge is dropped" $
        updateSamples 2000 50 [Sample (2000 - windowSecs - 1) 40]
          `shouldBe` [Sample 2000 50]
    context "abnormal" $ do
      it "a percentage drop keeps only the post-reset suffix" $
        updateSamples 100 5 [Sample 0 40, Sample 50 45]
          `shouldBe` [Sample 100 5]
      it "future samples are dropped (clock jump)" $
        updateSamples 100 41 [Sample 500 40] `shouldBe` [Sample 100 41]
    context "extreme" $ do
      it "a long plateau is retained" $
        updateSamples 180 40 [Sample 0 40, Sample 60 40, Sample 120 40]
          `shouldBe` [Sample 0 40, Sample 60 40, Sample 120 40, Sample 180 40]

  describe "predictExhaustion" $ do
    context "normal" $ do
      it "40% -> 50% over 600s exhausts 3000s after the newest sample" $
        predictExhaustion [Sample 0 40, Sample 600 50] `shouldBe` Just 3600
      it "intermediate samples do not disturb the endpoint fit" $
        predictExhaustion [Sample 0 40, Sample 300 42, Sample 600 50]
          `shouldBe` Just 3600
    context "boundary" $ do
      it "span of exactly 60s predicts" $
        predictExhaustion [Sample 0 40, Sample 60 41] `shouldBe` Just (60 + 59 * 60)
      it "span of 59s -> Nothing" $
        predictExhaustion [Sample 0 40, Sample 59 41] `shouldBe` Nothing
      it "already at 100% -> Nothing" $
        predictExhaustion [Sample 0 90, Sample 600 100] `shouldBe` Nothing
    context "abnormal" $ do
      it "single sample -> Nothing" $
        predictExhaustion [Sample 0 40] `shouldBe` Nothing
      it "flat percentage -> Nothing" $
        predictExhaustion [Sample 0 40, Sample 600 40] `shouldBe` Nothing
      it "empty -> Nothing" $ predictExhaustion [] `shouldBe` Nothing
    context "extreme" $ do
      it "near-vertical rate exhausts almost immediately" $
        predictExhaustion [Sample 0 50, Sample 60 99] `shouldBe` Just 62
      it "tiny rate still predicts, far in the future" $
        predictExhaustion [Sample 0 40, Sample 1800 41]
          `shouldBe` Just (1800 + 59 * 1800)
