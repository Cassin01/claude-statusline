-- | Burn-rate samples for the 5h rate limit. The stdin protocol only carries
-- a point-in-time used percentage, so each invocation persists (epoch, pct)
-- samples and the exhaustion prediction fits a rate over the retained window.
-- Samples are account-wide like the limit itself, so concurrent sessions
-- write mutually consistent data and a lost concurrent update is harmless.
module Statusline.RateSample
  ( Sample (..)
  , parseSamples
  , renderSamples
  , updateSamples
  , predictExhaustion
  , windowSecs
  ) where

import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Sample = Sample
  { sTime :: Integer
  , sPct :: Scientific
  }
  deriving (Eq, Show)

-- | How far back samples are retained (and how far the fit reaches).
windowSecs :: Integer
windowSecs = 1800

-- | An unchanged percentage is re-recorded at most this often, bounding both
-- file rewrites and file size while keeping plateaus visible to the fit.
heartbeatSecs :: Integer
heartbeatSecs = 60

-- | Predictions from a span shorter than this would extrapolate noise.
minSpanSecs :: Integer
minSpanSecs = 60

-- | One sample per line as "<epoch> <pct>". Malformed lines are skipped so
-- the store self-heals after a corrupt concurrent write.
parseSamples :: Text -> [Sample]
parseSamples = sortOn sTime . mapMaybe sample . T.lines
  where
    sample line = case T.words line of
      [t, p] -> do
        time <- readMaybe (T.unpack t)
        pct <- readMaybe (T.unpack p)
        if time < 0 then Nothing else Just (Sample time pct)
      _ -> Nothing

renderSamples :: [Sample] -> Text
renderSamples = T.unlines . map line
  where
    line (Sample t p) = T.pack (show t) <> " " <> T.pack (show p)

-- | Fold the current reading into the retained samples: drop what lies
-- outside the window or in the future (clock jump), record the reading when
-- it changed or the heartbeat elapsed, and keep only the maximal
-- non-decreasing suffix — a percentage drop means the 5h window rolled over,
-- so everything before the drop belongs to the previous window.
updateSamples :: Integer -> Scientific -> [Sample] -> [Sample]
updateSamples now pct old = risingSuffix (pruned <> appended)
  where
    pruned = filter (\s -> sTime s <= now && sTime s >= now - windowSecs) old
    appended = case reverse pruned of
      (latest : _)
        | sPct latest == pct && now - sTime latest < heartbeatSecs -> []
      _ -> [Sample now pct]

risingSuffix :: [Sample] -> [Sample]
risingSuffix = reverse . keep . reverse
  where
    keep (a : b : rest)
      | sPct b <= sPct a = a : keep (b : rest)
      | otherwise = [a]
    keep xs = xs

-- | Epoch second at which 100% is reached at the rate between the oldest and
-- newest retained samples. Nothing when there is no usable rate: fewer than
-- two samples, a span under 'minSpanSecs', a flat percentage, or already
-- at 100%.
predictExhaustion :: [Sample] -> Maybe Integer
predictExhaustion [] = Nothing
predictExhaustion samples@(oldest : _)
  | spanSecs >= minSpanSecs && rise > 0 && sPct newest < 100 =
      Just (sTime newest + ceiling (headroom * fromIntegral spanSecs / rise))
  | otherwise = Nothing
  where
    newest = last samples
    spanSecs = sTime newest - sTime oldest
    rise = toRealFloat (sPct newest - sPct oldest) :: Double
    headroom = toRealFloat (100 - sPct newest) :: Double
