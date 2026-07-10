-- Claude Code status line (four rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
--   row 4: ambient ticker (week forecast with moon · news), right-to-left scroll
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Statusline.Cache (cachedFetch)
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Moon (moonPhase)
import Statusline.News (newsTitles)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
import Statusline.Weather (forecastDays, openMeteoUrl, parseLocation, weekLine)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  input <- parseInput <$> BS.getContents
  columns <- columnsOr80 <$> lookupEnv "COLUMNS"
  home <- fmap T.pack <$> lookupEnv "HOME"
  branch <- gitBranch (T.unpack (effectiveCwd input))
  tokens <- readTokens (siTranscript input)
  zone <- resolveTimeZone (siResetsAt input)
  now <- round <$> getPOSIXTime
  loc <- cachedFetch "location" (24 * 3600) "https://ipinfo.io/json"
  forecast <- case parseLocation =<< loc of
    Just (lat, lon) -> cachedFetch "forecast" (3 * 3600) (openMeteoUrl lat lon)
    Nothing -> pure Nothing
  news <- cachedFetch "news" (20 * 60) "https://www.nhk.or.jp/rss/news/cat0.xml"
  -- until the forecast cache warms up, fall back to today's moon phase alone
  let weekItem = weekLine . forecastDays =<< forecast
      ticker = maybe [moonPhase now] pure weekItem <> maybe [] (take 3 . newsTitles) news
      env =
        Env
          { envColumns = columns
          , envHome = home
          , envBranch = branch
          , envTokens = tokens
          , envTimeZone = zone
          , envNow = now
          , envTicker = ticker
          }
  BS.putStr (encodeUtf8 (render env input))
