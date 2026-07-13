module Statusline.Input
  ( StatusInput (..)
  , emptyInput
  , parseInput
  , validEpoch
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value, decodeStrict)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific, isInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Statusline.Json (asNumber, asText, path)

-- | The subset of the Claude Code statusLine stdin protocol this tool reads.
data StatusInput = StatusInput
  { siCwd :: Maybe Text
  , siContextPct :: Maybe Scientific
  , siFiveHour :: Maybe Scientific
  , siSevenDay :: Maybe Scientific
  , siResetsAt :: Maybe Scientific
  , siTranscript :: Maybe FilePath
  , siModel :: Maybe Text
  , siEffort :: Maybe Text
  }
  deriving (Eq, Show)

emptyInput :: StatusInput
emptyInput = StatusInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Malformed JSON degrades to 'emptyInput' so row 1 still renders with the
-- "." cwd fallback, matching the bash original. Fields of an unexpected JSON
-- type are treated as absent rather than failing the whole parse.
parseInput :: ByteString -> StatusInput
parseInput = maybe emptyInput fromValue . decodeStrict

fromValue :: Value -> StatusInput
fromValue v =
  StatusInput
    { siCwd = asText =<< (path ["workspace", "current_dir"] v <|> path ["cwd"] v)
    , siContextPct = asNumber =<< path ["context_window", "used_percentage"] v
    , siFiveHour = asNumber =<< path ["rate_limits", "five_hour", "used_percentage"] v
    , siSevenDay = asNumber =<< path ["rate_limits", "seven_day", "used_percentage"] v
    , siResetsAt = asNumber =<< path ["rate_limits", "five_hour", "resets_at"] v
    , siTranscript = T.unpack <$> (asText =<< path ["transcript_path"] v)
    , siModel = asText =<< (path ["model", "display_name"] v <|> path ["model", "id"] v)
    , siEffort = asText =<< path ["effort", "level"] v
    }

-- | resets_at is honoured only as a non-negative integer epoch, mirroring the
-- bash digits-only guard.
validEpoch :: Scientific -> Maybe Integer
validEpoch sci
  | isInteger sci && sci >= 0 = Just (truncate sci)
  | otherwise = Nothing
