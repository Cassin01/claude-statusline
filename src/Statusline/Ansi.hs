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
  , sanitize
  ) where

import Data.ByteString qualified as BS
import Data.Char (chr, intToDigit, isControl, toUpper)
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

-- | Strip characters that can corrupt a terminal row: controls
-- (escape-sequence injection) and bidi overrides (a single RTL override
-- would visually reorder everything after it). ZWJ — also category Cf, like
-- the bidi characters — stays so emoji sequences keep rendering.
sanitize :: Text -> Text
sanitize = T.filter (\c -> not (isControl c || isBidiControl c))

isBidiControl :: Char -> Bool
isBidiControl c =
  c == '\x200E'
    || c == '\x200F'
    || (c >= '\x202A' && c <= '\x202E')
    || (c >= '\x2066' && c <= '\x2069')
