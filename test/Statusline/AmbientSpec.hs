module Statusline.AmbientSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Statusline.Ambient (buildTicker)
import Statusline.Moon (moonPhase)
import Statusline.Ticker (Span (..), plain)
import Test.Hspec

-- 2026-07-10 is a Friday; weather code 0 is clear sky.
sampleForecast :: Text
sampleForecast =
  "{\"daily\":{\"time\":[\"2026-07-10\"],\"weather_code\":[0],\"temperature_2m_max\":[30.2]}}"

item :: Int -> Text
item i = "<item><title>t" <> T.pack (show i) <> "</title></item>"

spec :: Spec
spec = describe "buildTicker" $ do
  it "leads with the week line when the forecast parses" $ do
    let ticker = buildTicker 0 (Just sampleForecast) []
    map spanUrl ticker `shouldBe` [Nothing]
    spanText (head ticker) `shouldSatisfy` T.isPrefixOf "金☀30°"
  it "no forecast -> today's moon phase alone" $
    buildTicker 0 Nothing [] `shouldBe` [plain (moonPhase 0)]
  it "malformed forecast -> moon phase fallback" $
    buildTicker 0 (Just "not json") [] `shouldBe` [plain (moonPhase 0)]
  it "labels each headline and keeps its link" $
    buildTicker 0 Nothing [("HN: ", Just "<item><title>t</title><link>https://e.com/a</link></item>")]
      `shouldBe` [plain (moonPhase 0), Span "HN: t" (Just "https://e.com/a")]
  it "feeds contribute in order" $
    map spanText (buildTicker 0 Nothing [("NHK: ", Just (item 1)), ("HN: ", Just (item 2))])
      `shouldBe` [moonPhase 0, "NHK: t1", "HN: t2"]
  it "caps each feed at three headlines" $
    map spanText (drop 1 (buildTicker 0 Nothing [("HN: ", Just (T.concat (map item [1 .. 5])))]))
      `shouldBe` ["HN: t1", "HN: t2", "HN: t3"]
  it "missing and malformed feeds contribute nothing" $
    buildTicker 0 Nothing [("NHK: ", Nothing), ("HN: ", Just "not xml")]
      `shouldBe` [plain (moonPhase 0)]
