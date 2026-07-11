module Statusline.TickerSpec (spec) where

import Data.Text (Text)
import Statusline.Ticker
import Test.Hspec

plain :: Text -> Span
plain t = Span t Nothing

linked :: Text -> Text -> Span
linked t u = Span t (Just u)

-- Flattened text of a windowed ticker, for tests that only care about
-- content, not span boundaries.
flat :: Int -> Integer -> Text -> Text
flat cols epoch content = foldMap spanText (marqueeSpans cols epoch [plain content])

spec :: Spec
spec = do
  describe "displayWidth" $ do
    it "ascii counts 1 per char" $ displayWidth "abc" `shouldBe` 3
    it "CJK counts 2 per char" $ displayWidth "日本語" `shouldBe` 6
    it "emoji counts 2" $ displayWidth "\x1F315" `shouldBe` 2
    it "variation selector and ZWJ count 0" $ displayWidth "\x2600\xFE0F\x200D" `shouldBe` 1
    it "emoji-presentation BMP weather symbols count 2" $ do
      displayWidth "⛅" `shouldBe` 2
      displayWidth "⛈" `shouldBe` 2
    it "text-presentation weather symbols count 1" $ do
      displayWidth "☀" `shouldBe` 1
      displayWidth "☁" `shouldBe` 1
    it "empty is 0" $ displayWidth "" `shouldBe` 0

  describe "marqueeSpans (scrolling)" $ do
    it "content that fits is static regardless of epoch" $ do
      flat 80 0 "short" `shouldBe` "short"
      flat 80 12345 "short" `shouldBe` "short"
    it "content exactly as wide as the columns is static (boundary)" $ do
      flat 5 0 "abcde" `shouldBe` "abcde"
      flat 5 99999 "abcde" `shouldBe` "abcde"
    it "epoch 0 starts at the head, clipped to columns" $
      flat 5 0 "abcdefghij" `shouldBe` "abcde"
    it "each second advances one code point" $
      flat 5 2 "abcdefghij" `shouldBe` "cdefg"
    it "wraps through the gap back to the head" $
      -- loop = content + three spaces = 13 code points
      flat 5 11 "abcdefghij" `shouldBe` "  abc"
    it "full cycle returns to the start" $
      flat 5 13 "abcdefghij" `shouldBe` flat 5 0 "abcdefghij"
    it "clips ascii to the column budget" $
      flat 3 0 "abcdef" `shouldBe` "abc"
    it "stops before a wide char that would straddle the edge" $
      flat 3 0 "日本語" `shouldBe` "日"
    it "wide chars never over-run the column budget" $
      displayWidth (flat 5 0 "日本語のニュース") `shouldSatisfy` (<= 5)
    it "a zero-width char is kept at an exhausted budget edge" $
      -- U+2600 (width 1) + VS-16 (width 0) must stay together at cols 1
      marqueeSpans 1 0 [linked "\x2600\xFE0F\&abc" "u"] `shouldBe` [linked "\x2600\xFE0F" "u"]
    it "empty content yields empty" $ flat 80 0 "" `shouldBe` ""
    it "non-positive columns yield empty" $ flat 0 0 "abc" `shouldBe` ""
    it "huge epoch still renders a window" $
      flat 5 999999999999 "abcdefghij" `shouldSatisfy` (not . (== ""))

  describe "marqueeSpans (link tracking)" $ do
    it "content that fits keeps span boundaries and URLs" $
      marqueeSpans 80 0 [linked "ab" "u1", plain " · ", linked "cd" "u2"]
        `shouldBe` [linked "ab" "u1", plain " · ", linked "cd" "u2"]
    it "adjacent spans with the same URL merge" $
      marqueeSpans 80 0 [linked "ab" "u", linked "cd" "u"] `shouldBe` [linked "abcd" "u"]
    it "the window slices a span but keeps its URL" $
      marqueeSpans 5 2 [linked "abcdefghij" "u"] `shouldBe` [linked "cdefg" "u"]
    it "a window across two items carries both URLs" $
      -- "abcdef" + gap = 9 cells; epoch 1 -> "bcde" split between the items
      marqueeSpans 4 1 [linked "abc" "u1", linked "def" "u2"]
        `shouldBe` [linked "bc" "u1", linked "de" "u2"]
    it "the wrap gap carries no URL" $
      -- "abcde" + gap = 8 cells; epoch 4 -> "e" plus the three-space gap
      marqueeSpans 4 4 [linked "abcde" "u"] `shouldBe` [linked "e" "u", plain "   "]
    it "empty spans yield nothing" $ marqueeSpans 10 0 [plain "", linked "" "u"] `shouldBe` []
