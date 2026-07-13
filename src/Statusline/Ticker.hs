module Statusline.Ticker
  ( Span (..)
  , plain
  , marqueeSpans
  , displayWidth
  ) where

import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Text qualified as T

-- | A run of ticker text, optionally linked to a URL and optionally carrying
-- its own ANSI color. Both are carried as data (not as embedded escape
-- sequences) so the marquee can slice content freely; the renderer turns them
-- into escape sequences after windowing.
data Span = Span
  { spanText :: Text
  , spanUrl :: Maybe Text
  , spanColor :: Maybe Text
  }
  deriving (Eq, Show)

-- | Span with no link and no color.
plain :: Text -> Span
plain t = Span t Nothing Nothing

-- | One code point annotated with its owning span's URL and color, so both
-- survive the scroll window and regroup afterwards.
type Cell = (Char, (Maybe Text, Maybe Text))

-- | Right-to-left ticker window. Content that fits is shown as-is; longer
-- content advances one code point per second of the given epoch clock,
-- wrapping through the given gap span. The window is clipped to the column
-- budget in display cells so wide (CJK/emoji) characters do not over-run,
-- splitting spans at the window edges while preserving each character's URL
-- and color.
marqueeSpans :: Int -> Integer -> Span -> [Span] -> [Span]
marqueeSpans cols epoch gap spans
  | cols <= 0 || null cells = []
  | cellsWidth cells <= cols = regroup cells
  | otherwise = regroup (takeCells cols (drop off looped <> take off looped))
  where
    cells = concatMap spanCells spans
    looped = cells <> spanCells gap
    off = fromIntegral (epoch `mod` fromIntegral (length looped))

spanCells :: Span -> [Cell]
spanCells s = [(c, (spanUrl s, spanColor s)) | c <- T.unpack (spanText s)]

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
regroup cells =
  [Span (T.pack (map fst g)) url color | g@((_, (url, color)) : _) <- groupBy ((==) `on` snd) cells]

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
