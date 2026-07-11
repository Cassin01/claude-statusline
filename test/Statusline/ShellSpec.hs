module Statusline.ShellSpec (spec) where

import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
import Statusline.RateSample (Sample (..))
import Statusline.Shell
import Statusline.Transcript (TokenTotals (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Hspec

initRepo :: FilePath -> IO ()
initRepo dir = do
  callProcess "git" ["-C", dir, "init", "-q"]
  callProcess
    "git"
    [ "-C", dir
    , "-c", "user.email=t@example.com"
    , "-c", "user.name=tester"
    , "commit", "-q", "--allow-empty", "-m", "init"
    ]

spec :: Spec
spec = do
  describe "gitBranch" $ do
    it "returns the current branch" $
      withSystemTempDirectory "statusline" $ \dir -> do
        initRepo dir
        callProcess "git" ["-C", dir, "checkout", "-q", "-b", "feat/x"]
        gitBranch dir `shouldReturn` Just "feat/x"

    it "falls back to short HEAD hash when detached" $
      withSystemTempDirectory "statusline" $ \dir -> do
        initRepo dir
        sha <- readProcess "git" ["-C", dir, "rev-parse", "HEAD"] ""
        callProcess "git" ["-C", dir, "checkout", "-q", "--detach"]
        branch <- gitBranch dir
        branch `shouldSatisfy` isJust
        T.unpack (fromJust branch) `shouldSatisfy` (`elem` [take n sha | n <- [4 .. 40]])

    it "non-git directory -> Nothing" $
      withSystemTempDirectory "statusline" $ \dir ->
        gitBranch dir `shouldReturn` Nothing

    it "nonexistent directory -> Nothing" $
      gitBranch "/does/not/exist" `shouldReturn` Nothing

  describe "readTokens" $ do
    it "sums a transcript file" $
      withSystemTempDirectory "statusline" $ \dir -> do
        let fp = dir <> "/transcript.jsonl"
        writeFile fp $
          unlines
            [ "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":100,\"cache_creation_input_tokens\":50,\"output_tokens\":30}}}"
            , "{\"type\":\"assistant\",\"message\":{\"usage\":{\"input_tokens\":200,\"output_tokens\":70,\"cache_read_input_tokens\":9999}}}"
            , "{\"type\":\"user\",\"message\":{\"usage\":{\"input_tokens\":5,\"output_tokens\":5}}}"
            ]
        readTokens (Just fp) `shouldReturn` TokenTotals 350 100

    it "missing file -> mempty" $
      readTokens (Just "/does/not/exist.jsonl") `shouldReturn` mempty

    it "no transcript path -> mempty" $
      readTokens Nothing `shouldReturn` mempty

  describe "rate sample store" $ do
    it "write/read round-trips" $
      withSystemTempDirectory "statusline" $ \dir -> do
        let samples = [Sample 100 40, Sample 200 50.5]
        writeRateSamples (Just dir) samples
        readRateSamples (Just dir) `shouldReturn` samples
    it "no cache dir -> read [] and write is a no-op" $ do
      readRateSamples Nothing `shouldReturn` []
      writeRateSamples Nothing [Sample 1 1]
    it "missing store file -> []" $
      withSystemTempDirectory "statusline" $ \dir ->
        readRateSamples (Just dir) `shouldReturn` []
    it "garbage lines are skipped" $
      withSystemTempDirectory "statusline" $ \dir -> do
        writeFile (dir <> "/rate5h") "garbage\n100 40\nx y\n"
        readRateSamples (Just dir) `shouldReturn` [Sample 100 40]
    it "write to a nonexistent dir does not throw" $
      writeRateSamples (Just "/does/not/exist") [Sample 1 1]

  describe "columnsOr80" $ do
    it "parses COLUMNS" $ columnsOr80 (Just "120") `shouldBe` 120
    it "unset -> 80" $ columnsOr80 Nothing `shouldBe` 80
    it "garbage -> 80" $ columnsOr80 (Just "wide") `shouldBe` 80
