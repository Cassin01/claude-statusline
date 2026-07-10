module Statusline.ShellSpec (spec) where

import Data.Maybe (fromJust, isJust)
import Data.Text qualified as T
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

  describe "columnsOr80" $ do
    it "parses COLUMNS" $ columnsOr80 (Just "120") `shouldBe` 120
    it "unset -> 80" $ columnsOr80 Nothing `shouldBe` 80
    it "garbage -> 80" $ columnsOr80 (Just "wide") `shouldBe` 80
