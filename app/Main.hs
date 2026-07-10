-- Claude Code status line (four rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
--   row 4: ambient ticker (weather · moon phase · news), right-to-left scroll
module Main (main) where

import Data.ByteString qualified as BS
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Statusline.Cache (cachedFetch)
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Moon (moonPhase)
import Statusline.News (newsTitles)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
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
  weather <- cachedFetch "weather" (30 * 60) "https://wttr.in/?format=%c+%t"
  news <- cachedFetch "news" (20 * 60) "https://www.nhk.or.jp/rss/news/cat0.xml"
  let ticker = catMaybes [weather] <> [moonPhase now] <> maybe [] (take 3 . newsTitles) news
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
