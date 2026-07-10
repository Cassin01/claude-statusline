module Statusline.TranscriptSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BLC
import Statusline.Transcript
import Test.Hspec

fixture :: [BLC.ByteString]
fixture =
  [ "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":100,\"cache_creation_input_tokens\":50,\"output_tokens\":30}}}"
  , "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":200,\"output_tokens\":70,\"cache_read_input_tokens\":9999}}}"
  , "{\"type\":\"user\",\"message\":{\"usage\":{\"input_tokens\":5,\"output_tokens\":5}}}"
  ]

spec :: Spec
spec = describe "sumTokens" $ do
  context "normal" $ do
    -- in = (100+50) + 200 = 350 ; out = 30 + 70 = 100 ; cache-read & non-assistant ignored
    it "sums assistant usage, cache-read excluded" $
      sumTokens fixture `shouldBe` TokenTotals 350 100

  context "boundary" $ do
    it "empty transcript" $ sumTokens [] `shouldBe` mempty
    it "cache-read-only line contributes nothing" $
      sumTokens ["{\"type\":\"assistant\",\"message\":{\"usage\":{\"cache_read_input_tokens\":9999}}}"]
        `shouldBe` mempty
    it "assistant line without usage" $
      sumTokens ["{\"type\":\"assistant\",\"message\":{}}"] `shouldBe` mempty

  context "abnormal" $ do
    it "malformed line skipped" $
      sumTokens ("{not json" : fixture) `shouldBe` TokenTotals 350 100
    it "blank line skipped" $
      sumTokens ("" : fixture) `shouldBe` TokenTotals 350 100

  context "extreme" $ do
    it "large counts don't overflow" $
      sumTokens
        [ "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":9007199254740993,\"output_tokens\":1}}}"
        , "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":9007199254740993,\"output_tokens\":1}}}"
        ]
        `shouldBe` TokenTotals 18014398509481986 2
