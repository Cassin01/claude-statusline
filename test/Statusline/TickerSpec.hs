module Statusline.TickerSpec (spec) where

import Data.Text qualified as T
import Statusline.Ticker
import Test.Hspec

spec :: Spec
spec = do
  describe "displayWidth" $ do
    it "ascii counts 1 per char" $ displayWidth "abc" `shouldBe` 3
    it "CJK counts 2 per char" $ displayWidth "日本語" `shouldBe` 6
    it "emoji counts 2" $ displayWidth "\x1F315" `shouldBe` 2
    it "variation selector and ZWJ count 0" $ displayWidth "\x2600\xFE0F\x200D" `shouldBe` 1
    it "empty is 0" $ displayWidth "" `shouldBe` 0

  describe "takeCells" $ do
    it "shorter text untouched" $ takeCells 10 "abc" `shouldBe` "abc"
    it "clips ascii to budget" $ takeCells 3 "abcdef" `shouldBe` "abc"
    it "stops before a wide char that would straddle the edge" $
      takeCells 3 "日本" `shouldBe` "日"
    it "zero budget yields empty" $ takeCells 0 "abc" `shouldBe` ""

  describe "marquee" $ do
    it "content that fits is static regardless of epoch" $ do
      marquee 80 0 "short" `shouldBe` "short"
      marquee 80 12345 "short" `shouldBe` "short"
    it "epoch 0 starts at the head, clipped to columns" $
      marquee 5 0 "abcdefghij" `shouldBe` "abcde"
    it "each second advances one code point" $
      marquee 5 2 "abcdefghij" `shouldBe` "cdefg"
    it "wraps through the gap back to the head" $
      -- loop = content + three spaces = 13 code points
      marquee 5 11 "abcdefghij" `shouldBe` "  abc"
    it "full cycle returns to the start" $
      marquee 5 13 "abcdefghij" `shouldBe` marquee 5 0 "abcdefghij"
    it "wide chars never over-run the column budget" $ do
      let win = marquee 5 0 "日本語のニュース"
      displayWidth win `shouldSatisfy` (<= 5)
    it "empty content yields empty" $ marquee 80 0 "" `shouldBe` ""
    it "non-positive columns yield empty" $ marquee 0 0 "abc" `shouldBe` ""
    it "huge epoch still renders a window" $
      T.length (marquee 5 999999999999 "abcdefghij") `shouldSatisfy` (> 0)
