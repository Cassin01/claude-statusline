module Statusline.CacheSpec (spec) where

import Data.Time (addUTCTime, getCurrentTime)
import Statusline.Cache (readCached)
import System.Directory (doesFileExist, setModificationTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "readCached" $ do
  it "missing file -> no content, refresh due, placeholder created" $
    withSystemTempDirectory "cache" $ \dir -> do
      let path = dir <> "/entry"
      readCached path 60 `shouldReturn` (Nothing, True)
      doesFileExist path `shouldReturn` True
  it "fresh file with content -> stripped content, no refresh" $
    withSystemTempDirectory "cache" $ \dir -> do
      let path = dir <> "/entry"
      writeFile path "hello\n"
      readCached path 60 `shouldReturn` (Just "hello", False)
  it "fresh but blank file -> no content, no refresh" $
    withSystemTempDirectory "cache" $ \dir -> do
      let path = dir <> "/entry"
      writeFile path "  \n"
      readCached path 60 `shouldReturn` (Nothing, False)
  it "stale file -> stale content still served, refresh due" $
    withSystemTempDirectory "cache" $ \dir -> do
      let path = dir <> "/entry"
      writeFile path "old"
      past <- addUTCTime (-120) <$> getCurrentTime
      setModificationTime path past
      readCached path 60 `shouldReturn` (Just "old", True)
  it "stale read claims the slot: the next read sees fresh" $
    withSystemTempDirectory "cache" $ \dir -> do
      let path = dir <> "/entry"
      writeFile path "old"
      past <- addUTCTime (-120) <$> getCurrentTime
      setModificationTime path past
      _ <- readCached path 60
      readCached path 60 `shouldReturn` (Just "old", False)
  it "unwritable path degrades to no content, no refresh" $
    readCached "/nonexistent-dir-claude-statusline/entry" 60
      `shouldReturn` (Nothing, False)
