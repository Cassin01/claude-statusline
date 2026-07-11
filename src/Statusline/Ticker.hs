module Statusline.Ticker
  ( Span (..)
  , marqueeSpans
  , displayWidth
  ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Text qualified as T

-- | A run of ticker text, optionally linked to a URL. The URL is carried as
-- data (not as embedded escape sequences) so the marquee can slice content
-- freely; the renderer turns it into an OSC 8 hyperlink after windowing.
data Span = Span
  { spanText :: Text
  , spanUrl :: Maybe Text
  }
  deriving (Eq, Show)

-- | One code point annotated with its owning span's URL, so link identity
-- survives the scroll window and regroups afterwards.
type Cell = (Char, Maybe Text)

-- | Right-to-left ticker window. Content that fits is shown as-is; longer
-- content advances one code point per second of the given epoch clock,
-- wrapping through a three-space gap (which carries no URL). The window is
-- clipped to the column budget in display cells so wide (CJK/emoji)
-- characters do not over-run, splitting spans at the window edges while
-- preserving each character's URL.
marqueeSpans :: Int -> Integer -> [Span] -> [Span]
marqueeSpans cols epoch spans
  | cols <= 0 || null cells = []
  | cellsWidth cells <= cols = regroup cells
  | otherwise = regroup (takeCells cols (drop off looped <> take off looped))
  where
    cells = [(c, spanUrl s) | s <- spans, c <- T.unpack (spanText s)]
    looped = cells <> map (,Nothing) "   "
    off = fromIntegral (epoch `mod` fromIntegral (length looped))

-- | Width in terminal cells: CJK and emoji count 2, joiners count 0.
displayWidth :: Text -> Int
displayWidth = T.foldl' (\acc c -> acc + charCells c) 0

cellsWidth :: [Cell] -> Int
cellsWidth = sum . map (charCells . fst)

-- | Longest prefix whose display width fits within the cell budget.
takeCells :: Int -> [Cell] -> [Cell]
takeCells _ [] = []
takeCells remaining (cell@(c, _) : cs)
  | w <= remaining = cell : takeCells (remaining - w) cs
  | otherwise = []
  where
    w = charCells c

regroup :: [Cell] -> [Span]
regroup cells = [Span (T.pack (map fst g)) url | g@((_, url) : _) <- groupBy ((==) `on` snd) cells]

charCells :: Char -> Int
charCells c
  | c == '\x200D' || c == '\xFE0F' = 0
  | wide = 2
  | otherwise = 1
  where
    wide =
      (c >= '\x1100' && c <= '\x115F')
        || (c >= '\x26C4' && c <= '\x26C8')
        || (c >= '\x2E80' && c <= '\xA4CF')
        || (c >= '\xAC00' && c <= '\xD7A3')
        || (c >= '\xF900' && c <= '\xFAFF')
        || (c >= '\xFE30' && c <= '\xFE4F')
        || (c >= '\xFF01' && c <= '\xFF60')
        || (c >= '\xFFE0' && c <= '\xFFE6')
        || (c >= '\x1F000' && c <= '\x1FAFF')
        || (c >= '\x20000' && c <= '\x3FFFD')
