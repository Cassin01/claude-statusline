module Statusline.Render
  ( Env (..)
  , render
  , effectiveCwd
  ) where

import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (TimeZone, defaultTimeLocale, formatTime, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Statusline.Ansi
import Statusline.Humanize (hum)
import Statusline.Input (StatusInput (..), validEpoch)
import Statusline.Ticker (Span (..), marqueeSpans, plain)
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
  , envTicker :: [Span]
  -- ^ Ambient items (weather, moon phase, news) for row 4, each optionally
  -- linking to a URL.
  }

-- | Full status line: rows joined by newlines, empty rows skipped, and no
-- trailing newline.
render :: Env -> StatusInput -> Text
render env input =
  T.intercalate "\n" . filter (not . T.null) $
    [row1 env input, row2 env input, row3 env input, row4 env]

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

-- row 3: 5h rate-limit reset clock time in the resolved local zone
row3 :: Env -> StatusInput -> Text
row3 env input = fromMaybe "" $ do
  secs <- validEpoch =<< siResetsAt input
  let local = utcToLocalTime (envTimeZone env) (posixSecondsToUTCTime (fromInteger secs))
      hhmm = T.pack (formatTime defaultTimeLocale "%H:%M" local)
  pure (withColor cyan ("5h resets at " <> hhmm))

-- row 4: ambient ticker scrolling right to left when wider than the terminal.
-- The row owner scrubs span text of controls and bidi overrides once, so no
-- producer can corrupt or reorder the terminal row. Color and OSC 8 links
-- are applied after windowing so the scroll never slices an escape sequence;
-- each visible run of a linked item gets its own hyperlink wrapper.
row4 :: Env -> Text
row4 env = case filter (not . T.null . spanText) (map scrub (envTicker env)) of
  [] -> ""
  items ->
    withColor dim . foldMap renderSpan $
      marqueeSpans (envColumns env) (envNow env) (intersperse gap items)
  where
    scrub s = s {spanText = sanitize (spanText s)}
    gap = plain " · "
    renderSpan (Span t url) = maybe t (`hyperlink` t) url

-- percentages arrive as e.g. 42.7 and are truncated like bash ${x%.*}
asPct :: RealFrac a => a -> Text
asPct p = tshow (truncate p :: Integer) <> "%"

tshow :: Show a => a -> Text
tshow = T.pack . show
