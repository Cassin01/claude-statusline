-- | Minimal navigation helpers for decoded aeson 'Value's.
module Statusline.Json
  ( path
  , asText
  , asNumber
  , asArray
  , asObject
  ) where

import Data.Aeson (Object, Value (..))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.Scientific (Scientific)
import Data.Text (Text)

path :: [Key] -> Value -> Maybe Value
path [] v = Just v
path (k : ks) (Object o) = path ks =<< KM.lookup k o
path _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

asNumber :: Value -> Maybe Scientific
asNumber (Number n) = Just n
asNumber _ = Nothing

asArray :: Value -> Maybe [Value]
asArray (Array xs) = Just (toList xs)
asArray _ = Nothing

asObject :: Value -> Maybe Object
asObject (Object o) = Just o
asObject _ = Nothing
