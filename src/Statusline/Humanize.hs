module Statusline.Humanize
  ( hum
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

-- | Human-readable token counts: 42 -> "42", 1500 -> "1.5k", 2500000 -> "2.50M".
hum :: Integer -> Text
hum n
  | n >= 1_000_000 = T.pack (printf "%.2fM" (fromIntegral n / 1e6 :: Double))
  | n >= 1_000 = T.pack (printf "%.1fk" (fromIntegral n / 1e3 :: Double))
  | otherwise = T.pack (show n)
