module Statusline.RenderSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (utc)
import Statusline.Input
import Statusline.Render
import Statusline.Ticker (Span (..))
import Statusline.Transcript (TokenTotals (..))
import Test.Hspec

defEnv :: Env
defEnv =
  Env
    { envColumns = 80
    , envHome = Just "/home/tester"
    , envBranch = Nothing
    , envTokens = mempty
    , envTimeZone = utc
    , envNow = 0
    , envTicker = []
    }

stripAnsi :: Text -> Text
stripAnsi t = case T.splitOn "\ESC[" t of
  [] -> ""
  (x : xs) -> x <> T.concat (map (T.drop 1 . T.dropWhile (/= 'm')) xs)

-- Drop OSC 8 hyperlink wrappers (BEL-terminated), keeping the link text.
stripOsc :: Text -> Text
stripOsc t = case T.splitOn "\ESC]8;" t of
  [] -> ""
  (x : xs) -> x <> T.concat (map (T.drop 1 . T.dropWhile (/= '\a')) xs)

-- Plain-text row n (0-based); "" when the row is absent.
rowAt :: Int -> Text -> Text
rowAt n t = case drop n (T.lines (stripAnsi (stripOsc t))) of
  (r : _) -> r
  [] -> ""

plain :: Text -> Span
plain t = Span t Nothing

withCtx :: Rational -> StatusInput
withCtx p = emptyInput {siContextPct = Just (fromRational p)}

spec :: Spec
spec = describe "render" $ do
  context "context percentage (value + colour thresholds)" $ do
    it "ctx 42 -> text" $ rowAt 1 (render defEnv (withCtx 42)) `shouldBe` "▣ 42%"
    it "ctx 42.7 -> decimal stripped" $ rowAt 1 (render defEnv (withCtx 42.7)) `shouldBe` "▣ 42%"
    it "ctx 49 -> blue" $ render defEnv (withCtx 49) `shouldSatisfy` T.isInfixOf "\ESC[34m▣ 49%"
    it "ctx 50 -> yellow (boundary)" $ render defEnv (withCtx 50) `shouldSatisfy` T.isInfixOf "\ESC[33m▣ 50%"
    it "ctx 65 -> yellow" $ render defEnv (withCtx 65) `shouldSatisfy` T.isInfixOf "\ESC[33m▣ 65%"
    it "ctx 80 -> red (boundary)" $ render defEnv (withCtx 80) `shouldSatisfy` T.isInfixOf "\ESC[31m▣ 80%"
    it "ctx 85 -> red" $ render defEnv (withCtx 85) `shouldSatisfy` T.isInfixOf "\ESC[31m▣ 85%"

  context "session limits" $ do
    it "5h + 7d" $
      rowAt 1 (render defEnv emptyInput {siFiveHour = Just 10, siSevenDay = Just 5})
        `shouldBe` "5h 10% 7d 5%"
    it "5h only" $
      rowAt 1 (render defEnv emptyInput {siFiveHour = Just 99}) `shouldBe` "5h 99%"
    it "7d only" $
      rowAt 1 (render defEnv emptyInput {siSevenDay = Just 3}) `shouldBe` "7d 3%"
    it "5h decimal stripped" $
      rowAt 1 (render defEnv emptyInput {siFiveHour = Just 12.9}) `shouldBe` "5h 12%"

  context "5h reset time (row 3, zone injected via Env)" $ do
    -- 1735723800 = 2025-01-01 09:30:00 UTC
    let resetInput = emptyInput {siFiveHour = Just 23, siResetsAt = Just 1735723800}
    it "resets_at -> row 3 clock time" $
      rowAt 2 (render defEnv resetInput) `shouldBe` "5h resets at 09:30"
    it "row 3 reset is cyan" $
      render defEnv resetInput `shouldSatisfy` T.isInfixOf "\ESC[36m5h resets at 09:30"
    it "no rate_limits -> no row 3" $
      rowAt 2 (render defEnv emptyInput) `shouldBe` ""
    it "5h without resets_at -> no row 3" $
      rowAt 2 (render defEnv emptyInput {siFiveHour = Just 99}) `shouldBe` ""
    it "fractional resets_at -> no row 3" $
      rowAt 2 (render defEnv emptyInput {siResetsAt = Just 1.5}) `shouldBe` ""
    it "negative resets_at -> no row 3" $
      rowAt 2 (render defEnv emptyInput {siResetsAt = Just (-1)}) `shouldBe` ""
    it "far-future epoch still formats HH:MM" $
      rowAt 1 (render defEnv emptyInput {siResetsAt = Just 9999999999})
        `shouldSatisfy` T.isPrefixOf "5h resets at "

  context "session tokens" $ do
    it "tokens summed, cache-read excluded" $
      rowAt 1 (render defEnv {envTokens = TokenTotals 350 100} emptyInput)
        `shouldBe` "Σ 450 ↑350 ↓100"
    it "zero tokens -> no token segment" $
      rowAt 1 (render defEnv emptyInput) `shouldBe` ""

  context "no status inputs" $ do
    it "empty input -> row 1 only, '.' fallback" $
      render defEnv emptyInput `shouldBe` "\ESC[2m.\ESC[0m"
    it "malformed json degrades to emptyInput" $
      parseInput "not valid json at all" `shouldBe` emptyInput

  context "git branch (row 1)" $ do
    let branched = defEnv {envBranch = Just "feat/x"}
    it "branch shown" $
      rowAt 0 (render branched emptyInput {siCwd = Just "/repo"}) `shouldBe` "⎇ feat/x /repo"
    it "branch is magenta" $
      render branched emptyInput `shouldSatisfy` T.isInfixOf "\ESC[35m⎇ feat/x"
    it "long branch mid-ellipsized to fit columns" $ do
      let long = T.replicate 120 "b"
          r0 = rowAt 0 (render branched {envBranch = Just long} emptyInput {siCwd = Just "/repo"})
      -- "⎇ " + branch + " " + "/repo" must fit within 80 columns
      T.length r0 `shouldBe` 80
      r0 `shouldSatisfy` T.isInfixOf "…"

  context "cwd path (row 1 always present)" $ do
    it "short path shown verbatim" $
      rowAt 0 (render defEnv emptyInput {siCwd = Just "/short"}) `shouldBe` "/short"
    it "home dir abbreviated to ~" $
      rowAt 0 (render defEnv emptyInput {siCwd = Just "/home/tester"}) `shouldBe` "~"
    it "home prefix abbreviated" $
      rowAt 0 (render defEnv emptyInput {siCwd = Just "/home/tester/proj"}) `shouldBe` "~/proj"
    it "home-alike without separator not abbreviated" $
      rowAt 0 (render defEnv emptyInput {siCwd = Just "/home/tester2"}) `shouldBe` "/home/tester2"
    it "long path head-trimmed to cap (min(cols/2, 40))" $ do
      let long = "/very/long/path" <> T.replicate 60 "/x"
          r0 = rowAt 0 (render defEnv emptyInput {siCwd = Just long})
      T.length r0 `shouldSatisfy` (<= 40)
      r0 `shouldSatisfy` T.isPrefixOf "…/"

  context "ticker (row 4)" $ do
    -- with emptyInput rows 2 and 3 are absent, so the ticker lands at index 1
    it "items joined with a dot separator" $
      rowAt 1 (render defEnv {envTicker = [plain "☀️ +31°C", plain "🌕 100%"]} emptyInput)
        `shouldBe` "☀️ +31°C · 🌕 100%"
    it "no ticker items -> no row" $
      rowAt 1 (render defEnv emptyInput) `shouldBe` ""
    it "blank items are dropped" $
      rowAt 1 (render defEnv {envTicker = [plain "", plain "🌕 100%"]} emptyInput)
        `shouldBe` "🌕 100%"
    it "row is dim" $
      render defEnv {envTicker = [plain "🌕 100%"]} emptyInput
        `shouldSatisfy` T.isInfixOf "\ESC[2m🌕 100%"
    it "over-long content is windowed to the columns" $ do
      let long = [plain (T.replicate 200 "x")]
          r = rowAt 1 (render defEnv {envTicker = long} emptyInput)
      T.length r `shouldBe` 80
    it "epoch advances the window" $ do
      let long = [plain (T.replicate 100 "a" <> T.replicate 100 "b")]
          at n = rowAt 1 (render defEnv {envTicker = long, envNow = n} emptyInput)
      at 0 `shouldNotBe` at 100
    it "linked items are wrapped in OSC 8 hyperlinks" $ do
      let item = Span "HN: headline" (Just "https://example.com/x")
      render defEnv {envTicker = [item]} emptyInput
        `shouldSatisfy` T.isInfixOf "\ESC]8;;https://example.com/x\aHN: headline\ESC]8;;\a"
    it "separators between linked items stay outside the links" $ do
      let items = [Span "a" (Just "u1"), Span "b" (Just "u2")]
      stripAnsi (render defEnv {envTicker = items} emptyInput)
        `shouldSatisfy` T.isSuffixOf "\ESC]8;;u1\aa\ESC]8;;\a · \ESC]8;;u2\ab\ESC]8;;\a"
    it "hyperlink escapes never consume window width" $ do
      let long = [Span (T.replicate 200 "x") (Just "https://example.com/x")]
          raw = render defEnv {envTicker = long, envNow = 190} emptyInput
      -- visible text is exactly the column budget even though the raw row
      -- carries the OSC 8 wrappers
      T.length (rowAt 1 raw) `shouldBe` 80
      raw `shouldSatisfy` T.isInfixOf "\ESC]8;;https://example.com/x\a"
    it "a window over the wrap gap re-opens the link on both runs" $ do
      -- 200 linked cells + 3-space gap; epoch 190 slices link/gap/link
      let long = [Span (T.replicate 200 "x") (Just "https://e.com")]
          raw = render defEnv {envTicker = long, envNow = 190} emptyInput
      T.count "\ESC]8;;https://e.com\a" raw `shouldBe` 2
    it "URI-reserved ascii in URLs passes through unencoded" $ do
      let item = Span "t" (Just "https://e.com/a?x=1&y=2#f")
      render defEnv {envTicker = [item]} emptyInput
        `shouldSatisfy` T.isInfixOf "\ESC]8;;https://e.com/a?x=1&y=2#f\a"
    it "non-ascii in URLs is percent-encoded for the OSC 8 URI" $ do
      -- 日 = U+65E5 = UTF-8 E6 97 A5
      let item = Span "t" (Just "https://e.com/日")
      render defEnv {envTicker = [item]} emptyInput
        `shouldSatisfy` T.isInfixOf "\ESC]8;;https://e.com/%E6%97%A5\a"

  context "composition across rows" $ do
    let full =
          emptyInput
            { siCwd = Just "/repo"
            , siContextPct = Just 30
            , siFiveHour = Just 10
            , siSevenDay = Just 5
            }
        fullEnv = defEnv {envBranch = Just "feat/x", envTokens = TokenTotals 350 100}
    it "everything: row 1 has branch" $
      rowAt 0 (render fullEnv full) `shouldSatisfy` T.isInfixOf "⎇ feat/x"
    it "everything: row 2 full segments" $
      rowAt 1 (render fullEnv full) `shouldBe` "5h 10% 7d 5% ▣ 30% Σ 450 ↑350 ↓100"
    it "branch present + ctx-only row 2" $
      rowAt 1 (render defEnv {envBranch = Just "feat/x"} (withCtx 30)) `shouldBe` "▣ 30%"
    it "limits + ctx joined on row 2" $
      rowAt 1 (render defEnv (withCtx 30) {siFiveHour = Just 10, siSevenDay = Just 5})
        `shouldBe` "5h 10% 7d 5% ▣ 30%"
    it "no trailing newline" $
      T.last (render fullEnv full) `shouldNotBe` '\n'
