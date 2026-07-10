module Statusline.HumanizeSpec (spec) where

import Statusline.Humanize (hum)
import Test.Hspec

-- The bash suite's `hum ''` / `hum abc` cases documented an awk string-coercion
-- quirk; they are untypeable with Integer and intentionally not ported.
spec :: Spec
spec = describe "hum" $ do
  context "normal" $ do
    it "42" $ hum 42 `shouldBe` "42"
    it "500" $ hum 500 `shouldBe` "500"
    it "1500 -> k" $ hum 1500 `shouldBe` "1.5k"
    it "2500000 -> M" $ hum 2500000 `shouldBe` "2.50M"

  context "boundary" $ do
    it "0" $ hum 0 `shouldBe` "0"
    it "999 (k floor -1)" $ hum 999 `shouldBe` "999"
    it "1000 (k floor)" $ hum 1000 `shouldBe` "1.0k"
    it "999500" $ hum 999500 `shouldBe` "999.5k"
    it "999999 (rounds up)" $ hum 999999 `shouldBe` "1000.0k"
    it "1000000 (M floor)" $ hum 1000000 `shouldBe` "1.00M"

  context "abnormal" $ do
    it "-5 (negative)" $ hum (-5) `shouldBe` "-5"

  context "extreme" $ do
    it "123456789" $ hum 123456789 `shouldBe` "123.46M"
    it "2147483647 (int32 max)" $ hum 2147483647 `shouldBe` "2147.48M"
    it "9999999999" $ hum 9999999999 `shouldBe` "10000.00M"
    it "5000000000000" $ hum 5000000000000 `shouldBe` "5000000.00M"
