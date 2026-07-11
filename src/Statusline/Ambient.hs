-- | Row-4 ambient ticker content: the week forecast (falling back to today's
-- moon phase until the forecast cache warms up) followed by up to a
-- configured number of labeled headlines per news feed.
module Statusline.Ambient
  ( buildTicker
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Statusline.Moon (moonPhase)
import Statusline.News (NewsItem (..), newsItems)
import Statusline.Ticker (Span (..), plain)
import Statusline.Weather (forecastDays, weekLine)

-- | Ticker spans from the headline cap per feed, the current epoch second,
-- the raw cached forecast JSON, and the raw cached feeds paired with their
-- display label.
buildTicker :: Int -> Integer -> Maybe Text -> [(Text, Maybe Text)] -> [Span]
buildTicker perFeed now forecast feeds =
  plain (fromMaybe (moonPhase now) (weekLine . forecastDays =<< forecast))
    : concatMap headlines feeds
  where
    headlines (label, feed) =
      [Span (label <> niTitle i) (niLink i) | i <- take perFeed (maybe [] newsItems feed)]
