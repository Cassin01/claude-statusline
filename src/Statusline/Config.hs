-- | User configuration from the XDG config dir
-- (@~\/.config\/claude-statusline\/config.json@). Every failure mode — missing
-- file, unreadable file, malformed JSON, wrong-typed keys — silently degrades
-- to the built-in defaults, per key where possible, so the status line never
-- fails or blocks. No on-screen indicator for a broken file: it would flash
-- on every refresh; validate with @jq . config.json@ instead.
module Statusline.Config
  ( Config (..)
  , Feed (..)
  , Rows (..)
  , Ttl (..)
  , defaultConfig
  , defaultRows
  , parseConfig
  , loadConfig
  , feedCacheName
  ) where

import Control.Exception (IOException, handle)
import Data.Aeson (Value, decodeStrict)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isAscii)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Statusline.Json (asArray, asBool, asNumber, asText, path)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import Text.Printf (printf)

data Feed = Feed
  { feedName :: Text
  , feedLabel :: Text
  , feedUrl :: Text
  }
  deriving (Eq, Show)

data Rows = Rows
  { rowGit :: Bool
  , rowUsage :: Bool
  , rowReset :: Bool
  , rowTicker :: Bool
  }
  deriving (Eq, Show)

data Ttl = Ttl
  { ttlLocation :: Int
  , ttlForecast :: Int
  , ttlNews :: Int
  }
  deriving (Eq, Show)

data Config = Config
  { cfgFeeds :: [Feed]
  , cfgHeadlineCount :: Int
  , cfgRows :: Rows
  , cfgTtl :: Ttl
  }
  deriving (Eq, Show)

defaultRows :: Rows
defaultRows = Rows True True True True

defaultConfig :: Config
defaultConfig =
  Config
    { cfgFeeds =
        [ Feed "nhk" "NHK: " "https://www.nhk.or.jp/rss/news/cat0.xml"
        , Feed "bbc" "BBC: " "https://feeds.bbci.co.uk/news/rss.xml"
        , Feed "hackernews" "HN: " "https://hnrss.org/frontpage"
        , Feed "zenn" "Zenn: " "https://zenn.dev/feed"
        ]
    , cfgHeadlineCount = 3
    , cfgRows = defaultRows
    , cfgTtl = Ttl (24 * 3600) (3 * 3600) (20 * 60)
    }

-- | Config file contents resolved against the defaults, per key. Total: any
-- input yields a usable 'Config'.
parseConfig :: ByteString -> Config
parseConfig = maybe defaultConfig fromValue . decodeStrict

fromValue :: Value -> Config
fromValue v =
  Config
    { cfgFeeds = fromMaybe (cfgFeeds defaultConfig) (mapMaybe feed <$> (asArray =<< path ["feeds"] v))
    , cfgHeadlineCount =
        fromMaybe (cfgHeadlineCount defaultConfig) (bound 0 20 <$> intAt ["headlineCount"])
    , cfgRows =
        Rows
          { rowGit = rowOr rowGit "git"
          , rowUsage = rowOr rowUsage "usage"
          , rowReset = rowOr rowReset "reset"
          , rowTicker = rowOr rowTicker "ticker"
          }
    , cfgTtl =
        Ttl
          { ttlLocation = ttlOr ttlLocation "location"
          , ttlForecast = ttlOr ttlForecast "forecast"
          , ttlNews = ttlOr ttlNews "news"
          }
    }
  where
    rowOr sel k = fromMaybe (sel defaultRows) (asBool =<< path ["rows", k] v)
    -- TTLs below 60s would spawn a curl on every refresh tick
    ttlOr sel k = fromMaybe (sel (cfgTtl defaultConfig)) (max 60 <$> intAt ["ttl", k])
    intAt ks = toBoundedInteger =<< asNumber =<< path ks v
    bound lo hi = max lo . min hi
    feed item = do
      name <- nonEmpty =<< asText =<< path ["name"] item
      url <- nonEmpty =<< asText =<< path ["url"] item
      pure (Feed name (fromMaybe (name <> ": ") (asText =<< path ["label"] item)) url)
    nonEmpty t = if T.null t then Nothing else Just t

-- | Read and parse the config file; 'defaultConfig' on any failure.
loadConfig :: IO Config
loadConfig = handle (\(_ :: IOException) -> pure defaultConfig) $ do
  dir <- getXdgDirectory XdgConfig "claude-statusline"
  let file = dir <> "/config.json"
  exists <- doesFileExist file
  if exists then parseConfig <$> BS.readFile file else pure defaultConfig

-- | Cache entry name for a feed. The name is user input and becomes a file
-- name, so it is sanitized to a safe alphabet (no traversal out of the cache
-- dir); the URL hash keeps entries distinct when names collide and retires
-- the old entry when only the URL changes.
feedCacheName :: Feed -> String
feedCacheName f = "feed-" <> sanitized <> "-" <> printf "%016x" (fnv1a64 (feedUrl f))
  where
    sanitized = take 32 (filter keep (T.unpack (feedName f)))
    keep c = isAscii c && (isAlphaNum c || c == '_' || c == '-')

fnv1a64 :: Text -> Word64
fnv1a64 = T.foldl' step 0xcbf29ce484222325
  where
    step h c = (h `xor` fromIntegral (fromEnum c)) * 0x100000001b3
