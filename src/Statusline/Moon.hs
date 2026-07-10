module Statusline.Moon
  ( moonPhase
  ) where

import Data.Fixed (mod')
import Data.Text (Text)
import Data.Text qualified as T

-- | Moon phase at the given epoch second as "emoji illumination%", computed
-- from the mean synodic month anchored at the 2000-01-06 18:14 UTC new moon.
moonPhase :: Integer -> Text
moonPhase epoch = emoji <> " " <> T.pack (show illum) <> "%"
  where
    f = (fromIntegral (epoch - newMoonEpoch) / synodicSecs) `mod'` 1 :: Double
    emoji = phases !! (round (f * 8) `mod` 8)
    illum = round (50 * (1 - cos (2 * pi * f))) :: Integer
    phases = ["\x1F311", "\x1F312", "\x1F313", "\x1F314", "\x1F315", "\x1F316", "\x1F317", "\x1F318"]

newMoonEpoch :: Integer
newMoonEpoch = 947182440

synodicSecs :: Double
synodicSecs = 29.530588853 * 86400
