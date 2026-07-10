-- | File cache with detached background refresh. The status line must never
-- block on the network, so reads always return whatever the cache holds and a
-- stale cache only triggers a fire-and-forget curl that lands for a later
-- invocation.
module Statusline.Cache
  ( cachedFetch
  , readCached
  ) where

import Control.Exception (IOException, handle)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory
  ( XdgDirectory (XdgCache)
  , createDirectoryIfMissing
  , doesFileExist
  , getModificationTime
  , getXdgDirectory
  , setModificationTime
  )
import System.Process

-- | Cached content for the named entry (under the XDG cache dir), refreshing
-- it in the background when older than the TTL. Nothing when the cache is
-- empty or unreadable.
cachedFetch :: String -> Int -> String -> IO (Maybe Text)
cachedFetch name ttlSecs url = handle (\(_ :: IOException) -> pure Nothing) $ do
  dir <- getXdgDirectory XdgCache "claude-statusline"
  createDirectoryIfMissing True dir
  let path = dir <> "/" <> name
  (content, refresh) <- readCached path ttlSecs
  when refresh (spawnCurl path url)
  pure content

-- | Cache content plus whether a refresh is due. Claims the refresh slot by
-- bumping the file mtime so concurrent invocations do not stampede.
readCached :: FilePath -> Int -> IO (Maybe Text, Bool)
readCached path ttlSecs = handle (\(_ :: IOException) -> pure (Nothing, False)) $ do
  exists <- doesFileExist path
  if not exists
    then do
      BS.writeFile path ""
      pure (Nothing, True)
    else do
      now <- getCurrentTime
      mtime <- getModificationTime path
      let stale = diffUTCTime now mtime > fromIntegral ttlSecs
      content <- nonEmpty . T.strip . decodeUtf8Lenient <$> BS.readFile path
      when stale (setModificationTime path now)
      pure (content, stale)
  where
    nonEmpty t = if T.null t then Nothing else Just t

-- | Fire-and-forget fetch: write to a temp file and rename so readers never
-- see a partial download. All streams detached; failures leave the cache as
-- it was and the next TTL expiry retries.
spawnCurl :: FilePath -> String -> IO ()
spawnCurl path url = handle (\(_ :: IOException) -> pure ()) $ do
  let tmp = path <> ".tmp"
      cmd =
        unwords
          ["curl -fsSL --max-time 8", quote url, "-o", quote tmp, "&& mv", quote tmp, quote path]
  _ <-
    createProcess
      (shell cmd)
        { std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        , new_session = True
        }
  pure ()
  where
    quote s = "'" <> s <> "'"
