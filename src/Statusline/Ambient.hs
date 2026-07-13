-- | Row-4 ambient ticker content: one item per forecast day (falling back to
-- today's moon phase until the forecast cache warms up) followed by up to a
-- configured number of labeled headlines per news feed.
module Statusline.Ambient
  ( buildTicker
  ) where

import Data.Text (Text)
import Statusline.Moon (moonPhase)
import Statusline.News (NewsItem (..), newsItems)
import Statusline.Ticker (Span (..), plain)
import Statusline.Weather (dayCells, forecastDays)

-- | Ticker spans from the headline cap per feed, the current epoch second,
-- the raw cached forecast JSON, and the raw cached feeds paired with their
-- display label.
buildTicker :: Int -> Integer -> Maybe Text -> [(Text, Maybe Text)] -> [Span]
buildTicker perFeed now forecast feeds = weather <> concatMap headlines feeds
  where
    weather = case maybe [] (dayCells . forecastDays) forecast of
      [] -> [plain (moonPhase now)]
      cells -> map plain cells
    headlines (label, feed) =
      [Span (label <> niTitle i) (niLink i) Nothing | i <- take perFeed (maybe [] newsItems feed)]
