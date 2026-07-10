module Statusline.Ticker
  ( marquee
  , displayWidth
  , takeCells
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Right-to-left ticker window. Content that fits is shown as-is; longer
-- content advances one code point per second of the given epoch clock,
-- wrapping through a three-space gap. The window is clipped to the column
-- budget in display cells so wide (CJK/emoji) characters do not over-run.
marquee :: Int -> Integer -> Text -> Text
marquee cols epoch content
  | cols <= 0 || T.null content = ""
  | displayWidth content <= cols = content
  | otherwise = takeCells cols (T.drop off looped <> T.take off looped)
  where
    looped = content <> "   "
    off = fromIntegral (epoch `mod` fromIntegral (T.length looped))

-- | Width in terminal cells: CJK and emoji count 2, joiners count 0.
displayWidth :: Text -> Int
displayWidth = T.foldl' (\acc c -> acc + charCells c) 0

-- | Longest prefix whose display width fits within the cell budget.
takeCells :: Int -> Text -> Text
takeCells cols t = T.pack (go cols (T.unpack t))
  where
    go _ [] = []
    go remaining (c : cs)
      | w <= remaining = c : go (remaining - w) cs
      | otherwise = []
      where
        w = charCells c

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
