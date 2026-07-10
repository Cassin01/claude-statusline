module Statusline.WeatherSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (fromGregorian)
import Statusline.Weather
import Test.Hspec

sampleForecast :: Text
sampleForecast =
  T.concat
    [ "{\"daily\":{"
    , "\"time\":[\"2026-07-10\",\"2026-07-11\",\"2026-07-12\"],"
    , "\"weather_code\":[0,3,61],"
    , "\"temperature_2m_max\":[30.5,29.4,28.0]}}"
    ]

spec :: Spec
spec = do
  describe "parseLocation" $ do
    it "extracts lat/lon from ipinfo loc" $
      parseLocation "{\"loc\":\"35.6895,139.6917\",\"city\":\"Tokyo\"}"
        `shouldBe` Just ("35.6895", "139.6917")
    it "negative coordinates accepted" $
      parseLocation "{\"loc\":\"-33.86,-151.2\"}" `shouldBe` Just ("-33.86", "-151.2")
    it "missing loc -> Nothing" $
      parseLocation "{\"city\":\"Tokyo\"}" `shouldBe` Nothing
    it "malformed JSON -> Nothing" $
      parseLocation "not json" `shouldBe` Nothing
    it "no comma -> Nothing" $
      parseLocation "{\"loc\":\"35.68\"}" `shouldBe` Nothing
    it "non-numeric parts rejected (guards the curl command line)" $ do
      parseLocation "{\"loc\":\"tokyo,japan\"}" `shouldBe` Nothing
      parseLocation "{\"loc\":\"35.68'; rm -rf /,139.69\"}" `shouldBe` Nothing
    it "empty part -> Nothing" $
      parseLocation "{\"loc\":\",139.69\"}" `shouldBe` Nothing

  describe "openMeteoUrl" $
    it "splices coordinates into the forecast URL" $
      openMeteoUrl "35.68" "139.69"
        `shouldBe` "https://api.open-meteo.com/v1/forecast?latitude=35.68&longitude=139.69&daily=weather_code,temperature_2m_max&timezone=auto&forecast_days=7"

  describe "forecastDays" $ do
    it "zips time, code and max temperature per day" $
      forecastDays sampleForecast
        `shouldBe` [ (fromGregorian 2026 7 10, 0, 30.5)
                   , (fromGregorian 2026 7 11, 3, 29.4)
                   , (fromGregorian 2026 7 12, 61, 28.0)
                   ]
    it "mismatched array lengths truncate to the shortest" $
      forecastDays
        "{\"daily\":{\"time\":[\"2026-07-10\",\"2026-07-11\"],\"weather_code\":[0],\"temperature_2m_max\":[30.5,29.4]}}"
        `shouldBe` [(fromGregorian 2026 7 10, 0, 30.5)]
    it "a day with a malformed field is dropped without shifting the rest" $
      forecastDays
        "{\"daily\":{\"time\":[\"2026-07-10\",\"bad\",\"2026-07-12\"],\"weather_code\":[0,1,2],\"temperature_2m_max\":[30.0,29.0,28.0]}}"
        `shouldBe` [(fromGregorian 2026 7 10, 0, 30.0), (fromGregorian 2026 7 12, 2, 28.0)]
    it "missing daily object -> no days" $
      forecastDays "{\"hourly\":{}}" `shouldBe` []
    it "malformed JSON -> no days" $ forecastDays "oops" `shouldBe` []
    it "empty arrays -> no days" $
      forecastDays "{\"daily\":{\"time\":[],\"weather_code\":[],\"temperature_2m_max\":[]}}"
        `shouldBe` []

  describe "weekLine" $ do
    -- 2000-01-06 (Thu) is the reference new moon; 2000-01-21 (Fri) is full
    it "weekday + weather emoji + rounded max temp + moon emoji" $
      weekLine [(fromGregorian 2000 1 6, 0, 34.2)] `shouldBe` Just "木☀34°\x1F311"
    it "rainy full-moon day" $
      weekLine [(fromGregorian 2000 1 21, 61, 5.6)] `shouldBe` Just "金🌧6°\x1F315"
    it "days joined by single spaces" $
      weekLine [(fromGregorian 2000 1 6, 0, 30), (fromGregorian 2000 1 7, 3, 28)]
        `shouldBe` Just "木☀30°\x1F311 金☁28°\x1F311"
    it "negative temperature keeps its sign" $
      weekLine [(fromGregorian 2000 1 6, 71, -5.4)] `shouldBe` Just "木🌨-5°\x1F311"
    it "no days -> Nothing" $ weekLine [] `shouldBe` Nothing
    it "extreme temperature still renders" $
      weekLine [(fromGregorian 2000 1 6, 0, 999.9)] `shouldBe` Just "木☀1000°\x1F311"

    context "WMO code grouping" $ do
      let emojiFor code = T.take 1 . T.drop 1 <$> weekLine [(fromGregorian 2000 1 6, code, 0)]
      it "0 clear" $ emojiFor 0 `shouldBe` Just "☀"
      it "1 mostly clear" $ emojiFor 1 `shouldBe` Just "🌤"
      it "2 partly cloudy" $ emojiFor 2 `shouldBe` Just "⛅"
      it "3 overcast" $ emojiFor 3 `shouldBe` Just "☁"
      it "45 fog" $ emojiFor 45 `shouldBe` Just "🌫"
      it "51 drizzle" $ emojiFor 51 `shouldBe` Just "🌦"
      it "61 rain" $ emojiFor 61 `shouldBe` Just "🌧"
      it "71 snow" $ emojiFor 71 `shouldBe` Just "🌨"
      it "80 showers" $ emojiFor 80 `shouldBe` Just "🌧"
      it "95 thunderstorm" $ emojiFor 95 `shouldBe` Just "⛈"
      it "unknown code falls back to cloud" $ emojiFor 42 `shouldBe` Just "☁"
