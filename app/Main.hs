-- Claude Code status line (four rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
--   row 4: ambient ticker (week forecast with moon · news headlines,
--          each headline an OSC 8 link), right-to-left scroll
-- Rows, feeds, and cache TTLs are user-configurable; see Statusline.Config.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Statusline.Ambient (buildTicker)
import Statusline.Cache (cacheDir, cachedFetch)
import Statusline.Config
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
import Statusline.Weather (openMeteoUrl, parseLocation)
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
  cfg <- loadConfig
  cache <- cacheDir
  let ttl = cfgTtl cfg
      tickerOn = rowTicker (cfgRows cfg)
  -- a disabled ticker skips every ambient fetch, not just the rendering
  loc <-
    if tickerOn
      then cachedFetch cache "location" (ttlLocation ttl) "https://ipinfo.io/json"
      else pure Nothing
  forecast <- case parseLocation =<< loc of
    Just (lat, lon) -> cachedFetch cache "forecast" (ttlForecast ttl) (openMeteoUrl lat lon)
    Nothing -> pure Nothing
  feeds <-
    if tickerOn
      then
        traverse
          (\f -> (feedLabel f,) <$> cachedFetch cache (feedCacheName f) (ttlNews ttl) (T.unpack (feedUrl f)))
          (cfgFeeds cfg)
      else pure []
  let env =
        Env
          { envColumns = columns
          , envHome = home
          , envBranch = branch
          , envTokens = tokens
          , envTimeZone = zone
          , envNow = now
          , envTicker = buildTicker (cfgHeadlineCount cfg) now forecast feeds
          , envRows = cfgRows cfg
          }
  BS.putStr (encodeUtf8 (render env input))
