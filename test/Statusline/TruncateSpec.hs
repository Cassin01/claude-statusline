module Statusline.TruncateSpec (spec) where

import Data.Text qualified as T
import Statusline.Truncate (midEllipsis, pathHeadTrim)
import Test.Hspec

spec :: Spec
spec = do
  describe "midEllipsis" $ do
    context "normal" $ do
      it "fits within max" $ midEllipsis 10 "main" `shouldBe` "main"
      it "truncate 24 -> 10" $ midEllipsis 10 "feature/long-branch-name" `shouldBe` "featu…name"
      it "truncate 10 -> 6" $ midEllipsis 6 "abcdefghij" `shouldBe` "abc…ij"

    context "boundary" $ do
      it "n == max (no cut)" $ midEllipsis 5 "abcde" `shouldBe` "abcde"
      it "n == max+1" $ midEllipsis 5 "abcdef" `shouldBe` "ab…ef"
      it "max == 3 (min for …)" $ midEllipsis 3 "abcdef" `shouldBe` "a…f"
      it "max == 2 (< 3, hard cut)" $ midEllipsis 2 "abcdef" `shouldBe` "ab"
      it "max == 0" $ midEllipsis 0 "abcdef" `shouldBe` ""
      it "empty string" $ midEllipsis 5 "" `shouldBe` ""
      it "single char, max 1" $ midEllipsis 1 "x" `shouldBe` "x"

    context "abnormal" $ do
      -- Pins the bash printf negative-precision quirk: whole string back.
      it "negative max (graceful)" $ midEllipsis (-1) "abcdef" `shouldBe` "abcdef"

    context "extreme" $ do
      it "100 chars -> 5" $ midEllipsis 5 (T.replicate 100 "a") `shouldBe` "aa…aa"
      it "max far exceeds length" $ midEllipsis 1000 "short" `shouldBe` "short"

  describe "pathHeadTrim" $ do
    context "normal" $ do
      it "fits within max" $ pathHeadTrim 20 "/a/b" `shouldBe` "/a/b"
      it "drop leading comps" $ pathHeadTrim 10 "/usr/local/bin" `shouldBe` "…/bin"
      it "keep tail component" $ pathHeadTrim 10 "src/app/main.go" `shouldBe` "…/main.go"

    context "boundary" $ do
      it "len == max (no trim)" $ pathHeadTrim 5 "abcde" `shouldBe` "abcde"
      it "len == max+1 (1 comp)" $ pathHeadTrim 5 "abcdef" `shouldBe` "…cdef"
      it "…/tail fits exactly" $ pathHeadTrim 5 "/a/bcd" `shouldBe` "…/bcd"

    context "abnormal" $ do
      it "max < 2 (hard cut)" $ pathHeadTrim 1 "aaaa" `shouldBe` "a"
      it "max == 0" $ pathHeadTrim 0 "aaaa" `shouldBe` ""

    context "extreme" $ do
      it "single long component" $ pathHeadTrim 10 "verylongfilename" `shouldBe` "…gfilename"
      it "many components -> tail" $ pathHeadTrim 8 "/a/b/c/d/e/f/g/h/final" `shouldBe` "…/final"
      it "max far exceeds length" $ pathHeadTrim 1000 "short" `shouldBe` "short"
