module Statusline.News
  ( newsTitles
  ) where

import Data.Text (Text)
import Data.Text qualified as T

-- | Item titles from an RSS feed, in document order. Channel-level titles are
-- ignored by only looking inside <item> blocks; CDATA wrappers and the five
-- predefined XML entities are resolved. Malformed input yields no titles.
newsTitles :: Text -> [Text]
newsTitles xml = filter (not . T.null) (map itemTitle (drop 1 (T.splitOn "<item>" xml)))
  where
    itemTitle item =
      clean . fst . T.breakOn "</title>" . T.drop titleLen . snd $ T.breakOn "<title>" item
    titleLen = T.length "<title>"

clean :: Text -> Text
clean = T.strip . unescape . unwrapCdata . T.strip

unwrapCdata :: Text -> Text
unwrapCdata t = case T.stripPrefix "<![CDATA[" t of
  Just rest -> maybe rest id (T.stripSuffix "]]>" rest)
  Nothing -> t

unescape :: Text -> Text
unescape =
  T.replace "&amp;" "&"
    . T.replace "&lt;" "<"
    . T.replace "&gt;" ">"
    . T.replace "&quot;" "\""
    . T.replace "&apos;" "'"
    . T.replace "&#39;" "'"
