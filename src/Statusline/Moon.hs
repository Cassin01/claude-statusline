module Statusline.Moon
  ( moonPhase
  , moonEmoji
  ) where

import Data.Fixed (mod')
import Data.Text (Text)
import Data.Text qualified as T

-- | Moon phase at the given epoch second as "emoji illumination%", computed
-- from the mean synodic month anchored at the 2000-01-06 18:14 UTC new moon.
moonPhase :: Integer -> Text
moonPhase epoch = moonEmoji epoch <> " " <> T.pack (show illum) <> "%"
  where
    illum = round (50 * (1 - cos (2 * pi * phaseFraction epoch))) :: Integer

-- | Phase emoji alone, for compact per-day listings.
moonEmoji :: Integer -> Text
moonEmoji epoch = phases !! (round (phaseFraction epoch * 8) `mod` 8)
  where
    phases = ["\x1F311", "\x1F312", "\x1F313", "\x1F314", "\x1F315", "\x1F316", "\x1F317", "\x1F318"]

-- | Position in the cycle, 0 = new moon, 0.5 = full.
phaseFraction :: Integer -> Double
phaseFraction epoch = (fromIntegral (epoch - newMoonEpoch) / synodicSecs) `mod'` 1

newMoonEpoch :: Integer
newMoonEpoch = 947182440

synodicSecs :: Double
synodicSecs = 29.530588853 * 86400
