module Statusline.Render
  ( Env (..)
  , render
  , effectiveCwd
  ) where

import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Statusline.Ansi
import Statusline.Config (Rows (..))
import Statusline.Humanize (hum)
import Statusline.Input (StatusInput (..), validEpoch)
import Statusline.RateSample (Sample, predictExhaustion)
import Statusline.Ticker (Span (..), marqueeSpans)
import Statusline.Transcript (TokenTotals (..), totalTokens)
import Statusline.Truncate (midEllipsis, pathHeadTrim)

-- | Everything the renderer needs from the outside world, resolved by the IO
-- shell so rendering stays pure and fully testable.
data Env = Env
  { envColumns :: Int
  , envHome :: Maybe Text
  , envBranch :: Maybe Text
  , envTokens :: TokenTotals
  , envTimeZone :: TimeZone
  , envNow :: Integer
  -- ^ Current epoch second, driving the row-4 marquee offset.
  , envTicker :: [[Span]]
  -- ^ Ambient items (weather, moon phase, news) for row 4. Each item is a list
  -- of spans (e.g. a colored feed tag followed by its headline); separators are
  -- placed between items, never within one. Spans optionally link to a URL.
  , envRows :: Rows
  -- ^ Which rows the user has enabled.
  , envSamples :: [Sample]
  -- ^ Retained 5h burn-rate samples, newest reading included, for the row-3
  -- exhaustion prediction.
  }

-- | Full status line: rows joined by newlines, empty rows skipped, and no
-- trailing newline.
render :: Env -> StatusInput -> Text
render env input =
  T.intercalate "\n" . filter (not . T.null) $
    [ gate rowGit (row1 env input)
    , gate rowUsage (row2 env input)
    , gate rowReset (row3 env input)
    , gate rowTicker (row4 env)
    ]
  where
    gate enabled row = if enabled (envRows env) then row else ""

effectiveCwd :: StatusInput -> Text
effectiveCwd input = case siCwd input of
  Just c | not (T.null c) -> c
  _ -> "."

-- row 1: magenta "⎇ branch" + dim cwd path, joined by a single space
row1 :: Env -> StatusInput -> Text
row1 env input = case envBranch env of
  Just branch ->
    let branchMax = max 8 (envColumns env - 3 - T.length disp)
     in withColor magenta ("⎇ " <> midEllipsis branchMax branch) <> " " <> pathSeg
  Nothing -> pathSeg
  where
    disp = pathHeadTrim (pathCap (envColumns env)) (abbrevHome (envHome env) (effectiveCwd input))
    pathSeg = withColor dim disp

pathCap :: Int -> Int
pathCap cols = max 10 (min 40 (cols `div` 2))

abbrevHome :: Maybe Text -> Text -> Text
abbrevHome (Just home) p
  | p == home = "~"
  | (home <> "/") `T.isPrefixOf` p = "~" <> T.drop (T.length home) p
abbrevHome _ p = p

-- row 2: limits · context · tokens, single-space joined
row2 :: Env -> StatusInput -> Text
row2 env input = T.intercalate " " (catMaybes [limitsSeg input, ctxSeg input, tokensSeg (envTokens env)])

limitsSeg :: StatusInput -> Maybe Text
limitsSeg input = case parts of
  [] -> Nothing
  ps -> Just (withColor cyan (T.intercalate " " ps))
  where
    parts =
      catMaybes
        [ (\p -> "5h " <> asPct p) <$> siFiveHour input
        , (\p -> "7d " <> asPct p) <$> siSevenDay input
        ]

ctxSeg :: StatusInput -> Maybe Text
ctxSeg input = do
  sci <- siContextPct input
  let p = truncate sci :: Integer
      color
        | p >= 80 = red
        | p >= 50 = yellow
        | otherwise = blue
  pure (withColor color ("▣ " <> tshow p <> "%"))

tokensSeg :: TokenTotals -> Maybe Text
tokensSeg toks@(TokenTotals tin tout)
  | totalTokens toks > 0 =
      Just $
        withColor green ("Σ " <> hum (totalTokens toks))
          <> " "
          <> withColor dim ("↑" <> hum tin <> " ↓" <> hum tout)
  | otherwise = Nothing

-- row 3: 5h rate-limit reset clock time in the resolved local zone, plus the
-- predicted 100% instant when the current burn rate reaches it before the
-- reset (a prediction at or past the reset is moot — the window rolls over
-- first).
row3 :: Env -> StatusInput -> Text
row3 env input = fromMaybe "" $ do
  secs <- validEpoch =<< siResetsAt input
  pure (withColor cyan ("5h resets at " <> clock env secs) <> exhaustSeg secs)
  where
    exhaustSeg resetAt = case predictExhaustion (envSamples env) of
      Just t | t < resetAt -> " " <> withColor yellow ("100% at ~" <> clock env t)
      _ -> ""

clock :: Env -> Integer -> Text
clock env secs =
  T.pack (formatTime defaultTimeLocale "%H:%M" local)
  where
    local = utcToLocalTime (envTimeZone env) (posixSecondsToUTCTime (fromInteger secs))

-- row 4: ambient ticker scrolling right to left when wider than the terminal.
-- Every separator — between items and at the marquee wrap — is the same cyan
-- middle dot, so it stands out against the dim item text. Separators go only
-- between items, never within one, so a multi-span item (e.g. a colored tag
-- plus its headline) reads as a unit. The row owner scrubs span text of
-- controls and bidi overrides once, so no producer can corrupt or reorder the
-- terminal row. Color and OSC 8 links are applied after windowing so the
-- scroll never slices an escape sequence; each visible run of a linked item
-- gets its own hyperlink wrapper.
row4 :: Env -> Text
row4 env = case mapMaybe cleanItem (envTicker env) of
  [] -> ""
  items ->
    foldMap renderSpan $
      marqueeSpans (envColumns env) (envNow env) sep (concat (intersperse [sep] items))
  where
    cleanItem spans = case filter (not . T.null . spanText) (map scrub spans) of
      [] -> Nothing
      ss -> Just ss
    scrub s = s {spanText = sanitize (spanText s)}
    sep = Span " · " Nothing (Just cyan)
    renderSpan (Span t url color) =
      withColor (fromMaybe dim color) (maybe t (`hyperlink` t) url)

-- percentages arrive as e.g. 42.7 and are truncated like bash ${x%.*}
asPct :: RealFrac a => a -> Text
asPct p = tshow (truncate p :: Integer) <> "%"

tshow :: Show a => a -> Text
tshow = T.pack . show
