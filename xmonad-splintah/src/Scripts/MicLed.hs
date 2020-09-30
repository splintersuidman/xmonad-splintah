-- | Toggle the microphone and update the LED accordingly.

module Scripts.MicLed
  ( micLed
  ) where

import Data.Char       (isSpace)
import Data.Function   ((&))
import Data.List       (isPrefixOf)
import XMonad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

micLed :: X ()
micLed = liftIO micLed'

micLed' :: IO ()
micLed' = do
  output <- runProcessWithInput "amixer" ["set", "Capture", "toggle"] ""
  let state = output
        & lines -- Split on newline
        & fmap (dropWhile isSpace) -- Drop leading space
        & filter ("Front Left:" `isPrefixOf`) -- Get lines which start with "Front Left:"
        & fmap (dropWhile (/= '[') . dropWhile (not . isSpace) . dropWhile (/= '['))
      brightness = case state of
        "[off]":_ -> 1
        _         -> 0

  print output
  print brightness
  safeSpawn "brightnessctl" ["--device=platform::micmute", "set", show brightness]
