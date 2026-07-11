module Statusline.News
  ( NewsItem (..)
  , newsItems
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Statusline.Ansi (sanitize)

-- | One RSS item: its title and, when present and plausible, its article
-- link.
data NewsItem = NewsItem
  { niTitle :: Text
  , niLink :: Maybe Text
  }
  deriving (Eq, Show)

-- | Items from an RSS feed, in document order. Channel-level tags are
-- ignored by only looking inside <item> blocks; CDATA wrappers and the five
-- predefined XML entities are resolved, control and bidi-override characters
-- are dropped. Items without a title are skipped; malformed input yields no
-- items.
newsItems :: Text -> [NewsItem]
newsItems xml =
  [ NewsItem title (itemLink item)
  | item <- drop 1 (T.splitOn "<item>" xml)
  , let title = tagText "title" item
  , not (T.null title)
  ]

-- | The item's <link> URL. Only absolute http(s) URLs of sane length without
-- embedded spaces qualify. Escape safety is enforced elsewhere: 'clean' has
-- already stripped control characters, and 'Statusline.Ansi.hyperlink'
-- percent-encodes whatever an OSC 8 URI cannot carry.
itemLink :: Text -> Maybe Text
itemLink item
  | isHttp && T.length url <= 2048 && not (" " `T.isInfixOf` url) = Just url
  | otherwise = Nothing
  where
    url = tagText "link" item
    isHttp = "http://" `T.isPrefixOf` url || "https://" `T.isPrefixOf` url

tagText :: Text -> Text -> Text
tagText tag item =
  clean . fst . T.breakOn ("</" <> tag <> ">") . T.drop open . snd $
    T.breakOn ("<" <> tag <> ">") item
  where
    open = T.length tag + 2

-- Sanitizing here (not only in the renderer) also keeps controls out of
-- links, so a feed cannot smuggle escape bytes into a URL.
clean :: Text -> Text
clean = T.strip . sanitize . unescape . unwrapCdata . T.strip

unwrapCdata :: Text -> Text
unwrapCdata t = case T.stripPrefix "<![CDATA[" t of
  Just rest -> fromMaybe rest (T.stripSuffix "]]>" rest)
  Nothing -> t

unescape :: Text -> Text
unescape =
  T.replace "&amp;" "&"
    . T.replace "&lt;" "<"
    . T.replace "&gt;" ">"
    . T.replace "&quot;" "\""
    . T.replace "&apos;" "'"
    . T.replace "&#39;" "'"
