module Statusline.Truncate
  ( midEllipsis
  , pathHeadTrim
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Middle-truncate with "…" so both ends of a long string stay visible.
-- A negative limit returns the string unchanged (parity with the bash
-- original, where printf treats a negative precision as omitted).
midEllipsis :: Int -> Text -> Text
midEllipsis limit s
  | T.length s <= limit = s
  | limit < 0 = s
  | limit < 3 = T.take limit s
  | otherwise = T.take headLen s <> "…" <> T.takeEnd tailLen s
  where
    keep = limit - 1
    headLen = (keep + 1) `div` 2
    tailLen = keep `div` 2

-- | Head-trim a path to fit the limit by dropping leading components,
-- prefixing "…/". A single over-long component becomes "…" plus its tail.
pathHeadTrim :: Int -> Text -> Text
pathHeadTrim limit s
  | T.length s <= limit = s
  | otherwise = go s
  where
    go rest =
      case T.breakOn "/" rest of
        (_, slashAndAfter)
          | T.null slashAndAfter -> lastComponent rest
          | otherwise ->
              let next = T.drop 1 slashAndAfter
               in if 2 + T.length next <= limit
                    then "…/" <> next
                    else go next
    lastComponent rest
      | limit < 0 = rest
      | limit < 2 = T.take limit rest
      | otherwise = "…" <> T.takeEnd (limit - 1) rest
