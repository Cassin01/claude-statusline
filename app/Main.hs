-- Claude Code status line (four rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
--   row 4: ambient ticker (week forecast with moon · NHK/HN/Zenn headlines,
--          each headline an OSC 8 link), right-to-left scroll
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Statusline.Ambient (buildTicker)
import Statusline.Cache (cacheDir, cachedFetch)
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
import Statusline.Weather (openMeteoUrl, parseLocation)
import System.Environment (lookupEnv)

-- cache entry name, row label, and feed URL per news source
newsFeeds :: [(String, Text, String)]
newsFeeds =
  [ ("news", "NHK: ", "https://www.nhk.or.jp/rss/news/cat0.xml")
  , ("hackernews", "HN: ", "https://hnrss.org/frontpage")
  , ("zenn", "Zenn: ", "https://zenn.dev/feed")
  ]

main :: IO ()
main = do
  input <- parseInput <$> BS.getContents
  columns <- columnsOr80 <$> lookupEnv "COLUMNS"
  home <- fmap T.pack <$> lookupEnv "HOME"
  branch <- gitBranch (T.unpack (effectiveCwd input))
  tokens <- readTokens (siTranscript input)
  zone <- resolveTimeZone (siResetsAt input)
  now <- round <$> getPOSIXTime
  cache <- cacheDir
  loc <- cachedFetch cache "location" (24 * 3600) "https://ipinfo.io/json"
  forecast <- case parseLocation =<< loc of
    Just (lat, lon) -> cachedFetch cache "forecast" (3 * 3600) (openMeteoUrl lat lon)
    Nothing -> pure Nothing
  feeds <- traverse (\(name, label, url) -> (label,) <$> cachedFetch cache name (20 * 60) url) newsFeeds
  let env =
        Env
          { envColumns = columns
          , envHome = home
          , envBranch = branch
          , envTokens = tokens
          , envTimeZone = zone
          , envNow = now
          , envTicker = buildTicker now forecast feeds
          }
  BS.putStr (encodeUtf8 (render env input))
