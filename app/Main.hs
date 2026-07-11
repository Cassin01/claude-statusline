-- Claude Code status line (four rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
--   row 4: ambient ticker (week forecast with moon · NHK/HN/Zenn headlines,
--          each headline an OSC 8 link), right-to-left scroll
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Statusline.Cache (cachedFetch)
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Moon (moonPhase)
import Statusline.News (NewsItem (..), newsItems)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
import Statusline.Ticker (Span (..))
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
  nhk <- cachedFetch "news" (20 * 60) "https://www.nhk.or.jp/rss/news/cat0.xml"
  hn <- cachedFetch "hackernews" (20 * 60) "https://hnrss.org/frontpage"
  zenn <- cachedFetch "zenn" (20 * 60) "https://zenn.dev/feed"
  -- until the forecast cache warms up, fall back to today's moon phase alone
  let weekItem = weekLine . forecastDays =<< forecast
      plain t = Span t Nothing
      headlines label feed =
        [Span (label <> niTitle i) (niLink i) | i <- take 3 (maybe [] newsItems feed)]
      ticker =
        maybe [plain (moonPhase now)] (pure . plain) weekItem
          <> headlines "NHK: " nhk
          <> headlines "HN: " hn
          <> headlines "Zenn: " zenn
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
