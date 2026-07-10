module Statusline.Transcript
  ( TokenTotals (..)
  , totalTokens
  , sumTokens
  ) where

import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)

-- | Cumulative session tokens. Input counts cache creation but excludes
-- cache reads to avoid double-counting.
data TokenTotals = TokenTotals
  { tokIn :: !Integer
  , tokOut :: !Integer
  }
  deriving (Eq, Show)

instance Semigroup TokenTotals where
  TokenTotals a b <> TokenTotals c d = TokenTotals (a + c) (b + d)

instance Monoid TokenTotals where
  mempty = TokenTotals 0 0

totalTokens :: TokenTotals -> Integer
totalTokens (TokenTotals i o) = i + o

-- | Sum usage over transcript JSONL lines. Only assistant messages count;
-- undecodable lines are skipped.
sumTokens :: [ByteString] -> TokenTotals
sumTokens = foldMap lineTotals

lineTotals :: ByteString -> TokenTotals
lineTotals line = fromMaybe mempty $ do
  v@(Object o) <- decode line
  String "assistant" <- KM.lookup "type" o
  let usage k = fromMaybe 0 $ do
        Number n <- KM.lookup k =<< objectAt ["message", "usage"] v
        pure (truncate n)
  pure $
    TokenTotals
      (usage "input_tokens" + usage "cache_creation_input_tokens")
      (usage "output_tokens")

objectAt :: [KM.Key] -> Value -> Maybe (KM.KeyMap Value)
objectAt [] (Object o) = Just o
objectAt (k : ks) (Object o) = objectAt ks =<< KM.lookup k o
objectAt _ _ = Nothing
