-- Claude Code status line (three rows):
--   row 1: git branch (left) · cwd path (right)
--   row 2: session (5h/7d) limits · context usage · session tokens
--   row 3: local clock time the 5h rate-limit window resets
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Statusline.Input (StatusInput (..), parseInput)
import Statusline.Render (Env (..), effectiveCwd, render)
import Statusline.Shell (columnsOr80, gitBranch, readTokens, resolveTimeZone)
import System.Environment (lookupEnv)

main :: IO ()
main = do
  input <- parseInput <$> BS.getContents
  columns <- columnsOr80 <$> lookupEnv "COLUMNS"
  home <- fmap T.pack <$> lookupEnv "HOME"
  branch <- gitBranch (T.unpack (effectiveCwd input))
  tokens <- readTokens (siTranscript input)
  zone <- resolveTimeZone (siResetsAt input)
  let env = Env {envColumns = columns, envHome = home, envBranch = branch, envTokens = tokens, envTimeZone = zone}
  BS.putStr (encodeUtf8 (render env input))
