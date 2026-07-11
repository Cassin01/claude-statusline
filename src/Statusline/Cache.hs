-- | File cache with detached background refresh. The status line must never
-- block on the network, so reads always return whatever the cache holds and a
-- stale cache only triggers a fire-and-forget curl that lands for a later
-- invocation.
module Statusline.Cache
  ( cacheDir
  , cachedFetch
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

-- | Our XDG cache dir, created if missing; resolve once per invocation and
-- pass to every 'cachedFetch'. Nothing when it cannot be created.
cacheDir :: IO (Maybe FilePath)
cacheDir = handle (\(_ :: IOException) -> pure Nothing) $ do
  dir <- getXdgDirectory XdgCache "claude-statusline"
  createDirectoryIfMissing True dir
  pure (Just dir)

-- | Cached content for the named entry under the given cache dir, refreshing
-- it in the background when older than the TTL. Nothing when the cache is
-- unavailable, empty, or unreadable.
cachedFetch :: Maybe FilePath -> String -> Int -> String -> IO (Maybe Text)
cachedFetch Nothing _ _ _ = pure Nothing
cachedFetch (Just dir) name ttlSecs url = handle (\(_ :: IOException) -> pure Nothing) $ do
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
-- it was and the next TTL expiry retries. The URL and paths ride as argv
-- positional parameters, never spliced into the shell string, so no caller
-- data can inject shell syntax.
spawnCurl :: FilePath -> String -> IO ()
spawnCurl path url = handle (\(_ :: IOException) -> pure ()) $ do
  let fetch = "curl -fsSL --max-time 8 \"$1\" -o \"$2\" && mv \"$2\" \"$3\""
  _ <-
    createProcess
      (proc "sh" ["-c", fetch, "claude-statusline", url, path <> ".tmp", path])
        { std_in = NoStream
        , std_out = NoStream
        , std_err = NoStream
        , new_session = True
        }
  pure ()
