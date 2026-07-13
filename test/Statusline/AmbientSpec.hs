module Statusline.AmbientSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Statusline.Ambient (buildTicker)
import Statusline.Ansi (blue, green, magenta, red, yellow)
import Statusline.Moon (moonPhase)
import Statusline.Ticker (Span (..), plain)
import Test.Hspec

-- 2026-07-10 is a Friday; weather code 0 is clear sky.
sampleForecast :: Text
sampleForecast =
  "{\"daily\":{\"time\":[\"2026-07-10\"],\"weather_code\":[0],\"temperature_2m_max\":[30.2]}}"

twoDayForecast :: Text
twoDayForecast =
  "{\"daily\":{\"time\":[\"2026-07-10\",\"2026-07-11\"],\"weather_code\":[0,3],\"temperature_2m_max\":[30.2,28.0]}}"

item :: Int -> Text
item i = "<item><title>t" <> T.pack (show i) <> "</title></item>"

spec :: Spec
spec = describe "buildTicker" $ do
  it "leads with the forecast days when the forecast parses" $ do
    let ticker = buildTicker 3 0 (Just sampleForecast) []
    map spanUrl ticker `shouldBe` [Nothing]
    spanText (head ticker) `shouldSatisfy` T.isPrefixOf "金☀30°"
  it "each forecast day is its own item" $ do
    let texts = map spanText (buildTicker 3 0 (Just twoDayForecast) [])
    map (T.take 5) texts `shouldBe` ["金☀30°", "土☁28°"]
  it "no forecast -> today's moon phase alone" $
    buildTicker 3 0 Nothing [] `shouldBe` [plain (moonPhase 0)]
  it "malformed forecast -> moon phase fallback" $
    buildTicker 3 0 (Just "not json") [] `shouldBe` [plain (moonPhase 0)]
  it "labels each headline with a colored tag span sharing the link" $
    buildTicker 3 0 Nothing [("HN: ", Just "<item><title>t</title><link>https://e.com/a</link></item>")]
      `shouldBe` [ plain (moonPhase 0)
                 , Span "HN: " (Just "https://e.com/a") (Just yellow)
                 , Span "t" (Just "https://e.com/a") Nothing
                 ]
  it "feeds contribute in order" $
    map spanText (buildTicker 3 0 Nothing [("NHK: ", Just (item 1)), ("HN: ", Just (item 2))])
      `shouldBe` [moonPhase 0, "NHK: ", "t1", "HN: ", "t2"]
  it "tag colors cycle through the palette by feed position" $ do
    let feeds = [(T.pack (show n) <> ": ", Just (item n)) | n <- [1 .. 6 :: Int]]
        tagColors = [c | Span _ _ (Just c) <- buildTicker 3 0 Nothing feeds]
    tagColors `shouldBe` [yellow, green, magenta, blue, red, yellow]
  it "headlines of one feed share its tag color" $ do
    let ticker = drop 1 (buildTicker 3 0 Nothing [("HN: ", Just (item 1 <> item 2))])
    map spanColor ticker `shouldBe` [Just yellow, Nothing, Just yellow, Nothing]
  it "caps each feed at the given headline count" $
    map spanText (drop 1 (buildTicker 3 0 Nothing [("HN: ", Just (T.concat (map item [1 .. 5])))]))
      `shouldBe` ["HN: ", "t1", "HN: ", "t2", "HN: ", "t3"]
  it "headline count 1 keeps only the first item per feed" $
    map spanText (drop 1 (buildTicker 1 0 Nothing [("HN: ", Just (T.concat (map item [1 .. 5])))]))
      `shouldBe` ["HN: ", "t1"]
  it "headline count 0 drops all headlines" $
    buildTicker 0 0 Nothing [("HN: ", Just (item 1))] `shouldBe` [plain (moonPhase 0)]
  it "missing and malformed feeds contribute nothing" $
    buildTicker 3 0 Nothing [("NHK: ", Nothing), ("HN: ", Just "not xml")]
      `shouldBe` [plain (moonPhase 0)]
