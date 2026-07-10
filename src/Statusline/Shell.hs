-- | The IO shell: every effect the renderer needs, resolved up front.
module Statusline.Shell
  ( gitBranch
  , readTokens
  , resolveTimeZone
  , columnsOr80
  ) where

import Control.Exception (IOException, handle)
import Data.Maybe (fromMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (TimeZone, getTimeZone, utc)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Statusline.Input (validEpoch)
import Statusline.Transcript (TokenTotals, sumTokens)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process
import Text.Read (readMaybe)

-- | Current branch, falling back to the short HEAD hash when detached.
-- Nothing when the directory is not a repository or git is unavailable.
gitBranch :: FilePath -> IO (Maybe Text)
gitBranch dir = do
  branch <- runGit ["-C", dir, "branch", "--show-current"]
  case nonEmpty branch of
    Just b -> pure (Just b)
    Nothing -> nonEmpty <$> runGit ["-C", dir, "rev-parse", "--short", "HEAD"]
  where
    nonEmpty t = t >>= \s -> if T.null s then Nothing else Just s

runGit :: [String] -> IO (Maybe Text)
runGit args = handle (\(_ :: IOException) -> pure Nothing) $ do
  (_, Just out, Just err, ph) <-
    createProcess (proc "git" args) {std_out = CreatePipe, std_err = CreatePipe}
  stdout' <- BS.hGetContents out
  hClose err
  code <- waitForProcess ph
  pure $ case code of
    ExitSuccess -> Just (T.strip (decodeUtf8Lenient stdout'))
    ExitFailure _ -> Nothing

readTokens :: Maybe FilePath -> IO TokenTotals
readTokens Nothing = pure mempty
readTokens (Just fp) = handle (\(_ :: IOException) -> pure mempty) $ do
  exists <- doesFileExist fp
  if exists
    then sumTokens . BLC.lines . BL.fromStrict <$> BS.readFile fp
    else pure mempty

-- | Zone in effect at the reset instant (honours TZ); utc when absent.
resolveTimeZone :: Maybe Scientific -> IO TimeZone
resolveTimeZone resetsAt = case validEpoch =<< resetsAt of
  Just secs -> getTimeZone (posixSecondsToUTCTime (fromInteger secs))
  Nothing -> pure utc

columnsOr80 :: Maybe String -> Int
columnsOr80 s = fromMaybe 80 (readMaybe =<< s)
