-- | Week-ahead forecast line: ipinfo.io geolocation JSON -> Open-Meteo daily
-- forecast JSON -> one per-day segment of weekday, weather emoji, max
-- temperature, and moon phase.
module Statusline.Weather
  ( parseLocation
  , openMeteoUrl
  , forecastDays
  , weekLine
  ) where

import Data.Aeson (Value (..), decodeStrict)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, DayOfWeek (..), UTCTime (..), dayOfWeek)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Statusline.Moon (moonEmoji)

-- | Latitude and longitude from an ipinfo.io response ("loc": "35.68,139.69").
-- Both parts must be strictly numeric: the values are spliced into the curl
-- command line, so anything else is rejected.
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
forecastDays raw = maybe [] id $ do
  v <- decodeStrict (encodeUtf8 raw)
  times <- asArray =<< path ["daily", "time"] v
  codes <- asArray =<< path ["daily", "weather_code"] v
  temps <- asArray =<< path ["daily", "temperature_2m_max"] v
  pure (mapMaybe triple (zip3 times codes temps))
  where
    triple (t, c, m) =
      (,,)
        <$> (parseDay =<< asText t)
        <*> (asInt =<< asNumber c)
        <*> (toRealFloat <$> asNumber m)
    parseDay = iso8601ParseM . T.unpack
    asInt s = Just (round (toRealFloat s :: Double))

-- | "金☀34°🌖 土☀35°🌗 …" — weekday, weather, rounded max temperature, and
-- the moon phase at that day's noon UTC. Nothing when there are no days.
weekLine :: [(Day, Int, Double)] -> Maybe Text
weekLine [] = Nothing
weekLine days = Just (T.intercalate " " (map dayCell days))
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
  | code == 0 = "☀"
  | code == 1 = "🌤"
  | code == 2 = "⛅"
  | code == 3 = "☁"
  | code `elem` [45, 48] = "🌫"
  | code >= 51 && code <= 57 = "🌦"
  | code >= 61 && code <= 67 = "🌧"
  | (code >= 71 && code <= 77) || code `elem` [85, 86] = "🌨"
  | code >= 80 && code <= 82 = "🌧"
  | code >= 95 = "⛈"
  | otherwise = "☁"

path :: [Key] -> Value -> Maybe Value
path [] v = Just v
path (k : ks) (Object o) = path ks =<< KM.lookup k o
path _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

asNumber :: Value -> Maybe Scientific
asNumber (Number n) = Just n
asNumber _ = Nothing

asArray :: Value -> Maybe [Value]
asArray (Array xs) = Just (toList xs)
asArray _ = Nothing
