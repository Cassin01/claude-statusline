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
  ) where

import Data.Text (Text)

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
