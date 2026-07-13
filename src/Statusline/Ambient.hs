-- | Row-4 ambient ticker content: one item per forecast day (falling back to
-- today's moon phase until the forecast cache warms up) followed by up to a
-- configured number of labeled headlines per news feed.
module Statusline.Ambient
  ( buildTicker
  ) where

import Data.Text (Text)
import Statusline.Ansi (blue, green, magenta, red, yellow)
import Statusline.Moon (moonPhase)
import Statusline.News (NewsItem (..), newsItems)
import Statusline.Ticker (Span (..), plain)
import Statusline.Weather (dayCells, forecastDays)

-- | Ticker items from the headline cap per feed, the current epoch second, the
-- raw cached forecast JSON, and the raw cached feeds paired with their display
-- label. Each item is a list of spans the renderer keeps together (no separator
-- inserted within it). A headline item is two spans: the feed label, colored
-- from 'tagPalette' by feed position so feeds are distinguishable at a glance,
-- followed by the title in the default (dim) color; both share the link.
buildTicker :: Int -> Integer -> Maybe Text -> [(Text, Maybe Text)] -> [[Span]]
buildTicker perFeed now forecast feeds =
  weather <> concat (zipWith headlines (cycle tagPalette) feeds)
  where
    weather = case maybe [] (dayCells . forecastDays) forecast of
      [] -> [[plain (moonPhase now)]]
      cells -> [[plain c] | c <- cells]
    headlines color (label, feed) =
      [ [Span label (niLink i) (Just color), Span (niTitle i) (niLink i) Nothing]
      | i <- take perFeed (maybe [] newsItems feed)
      ]

-- | Feed label colors. Cyan is excluded: row 4 uses it for the separators.
tagPalette :: [Text]
tagPalette = [yellow, green, magenta, blue, red]
