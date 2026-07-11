module Statusline.Ansi
  ( reset
  , dim
  , blue
  , cyan
  , yellow
  , red
  , magenta
  , green
  , withColor
  , hyperlink
  ) where

import Data.ByteString qualified as BS
import Data.Char (chr, intToDigit, toUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

reset, dim, blue, cyan, yellow, red, magenta, green :: Text
reset = "\ESC[0m"
dim = "\ESC[2m"
blue = "\ESC[34m"
cyan = "\ESC[36m"
yellow = "\ESC[33m"
red = "\ESC[31m"
magenta = "\ESC[35m"
green = "\ESC[32m"

withColor :: Text -> Text -> Text
withColor color s = color <> s <> reset

-- | OSC 8 hyperlink (BEL-terminated, the form the Claude Code docs use).
-- Terminals without OSC 8 support show the text unchanged. Bytes an OSC 8
-- URI cannot carry (controls, space, DEL, non-ASCII) are percent-encoded, so
-- no URL can terminate or corrupt the sequence.
hyperlink :: Text -> Text -> Text
hyperlink url s = "\ESC]8;;" <> escapeUri url <> "\a" <> s <> "\ESC]8;;\a"

escapeUri :: Text -> Text
escapeUri = T.pack . concatMap encodeByte . BS.unpack . encodeUtf8
  where
    encodeByte b
      | b > 0x20 && b < 0x7F = [chr (fromIntegral b)]
      | otherwise = ['%', hex (b `div` 16), hex (b `mod` 16)]
    hex = toUpper . intToDigit . fromIntegral
