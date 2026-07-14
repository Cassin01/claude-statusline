-- | Week-ahead forecast cells: ipinfo.io geolocation JSON -> Open-Meteo daily
-- forecast JSON -> one per-day segment of weekday, weather emoji, max
-- temperature, and moon phase.
module Statusline.Weather
  ( parseLocation
  , openMeteoUrl
  , forecastDays
  , dayCells
  ) where

import Data.Aeson (decodeStrict)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, DayOfWeek (..), UTCTime (..), dayOfWeek)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Statusline.Json (asArray, asNumber, asText, path)
import Statusline.Moon (moonEmoji)

-- | Latitude and longitude from an ipinfo.io response ("loc": "35.68,139.69").
-- Both parts must be strictly numeric: the values are spliced into the
-- Open-Meteo URL, so anything else is rejected.
parseLocation :: Text -> Maybe (Text, Text)
parseLocation raw = do
  v <- decodeStrict (encodeUtf8 raw)
  loc <- asText =<< path ["loc"] v
  case T.splitOn "," loc of
    [lat, lon] | coord lat && coord lon -> Just (lat, lon)
    _ -> Nothing
  where
    coord t = not (T.null t) && T.all (`elem` ("0123456789.-" :: String)) t

openMeteoUrl :: Text -> Text -> String
openMeteoUrl lat lon =
  "https://api.open-meteo.com/v1/forecast?latitude="
    <> T.unpack lat
    <> "&longitude="
    <> T.unpack lon
    <> "&daily=weather_code,temperature_2m_max&timezone=auto&forecast_days=7"

-- | (date, WMO weather code, max temperature) per forecast day. Arrays are
-- zipped positionally; a day with any malformed field is dropped whole so the
-- remaining triples stay aligned. Malformed JSON yields no days.
forecastDays :: Text -> [(Day, Int, Double)]
forecastDays raw = fromMaybe [] $ do
  daily <- path ["daily"] =<< decodeStrict (encodeUtf8 raw)
  times <- asArray =<< path ["time"] daily
  codes <- asArray =<< path ["weather_code"] daily
  temps <- asArray =<< path ["temperature_2m_max"] daily
  pure (mapMaybe triple (zip3 times codes temps))
  where
    triple (t, c, m) =
      (,,)
        <$> (parseDay =<< asText t)
        <*> (asInt <$> asNumber c)
        <*> (toRealFloat <$> asNumber m)
    parseDay = iso8601ParseM . T.unpack
    asInt s = round (toRealFloat s :: Double)

-- | One "金☀34°🌖" cell per day — weekday, weather, rounded max temperature,
-- and the moon phase at that day's noon UTC — left unjoined so the ticker can
-- put its own separator between days.
dayCells :: [(Day, Int, Double)] -> [Text]
dayCells = map dayCell
  where
    dayCell (d, code, tmax) =
      weekdayKanji (dayOfWeek d)
        <> wmoEmoji code
        <> T.pack (show (round tmax :: Integer))
        <> "°"
        <> moonEmoji (noonEpoch d)
    noonEpoch d = round (utcTimeToPOSIXSeconds (UTCTime d 43200))

weekdayKanji :: DayOfWeek -> Text
weekdayKanji d = case d of
  Monday -> "月"
  Tuesday -> "火"
  Wednesday -> "水"
  Thursday -> "木"
  Friday -> "金"
  Saturday -> "土"
  Sunday -> "日"

-- WMO weather interpretation codes, grouped to the nearest emoji.
wmoEmoji :: Int -> Text
wmoEmoji code
  | code == 0 = "☀\xFE0F"
  | code == 1 = "🌤\xFE0F"
  | code == 2 = "⛅\xFE0F"
  | code == 3 = "☁\xFE0F"
  | code `elem` [45, 48] = "🌫\xFE0F"
  | code >= 51 && code <= 57 = "🌦\xFE0F"
  | code >= 61 && code <= 67 = "🌧\xFE0F"
  | (code >= 71 && code <= 77) || code `elem` [85, 86] = "🌨\xFE0F"
  | code >= 80 && code <= 82 = "🌧\xFE0F"
  | code >= 95 = "⛈\xFE0F"
  | otherwise = "☁\xFE0F"
