module Scripts.MpvClip
  ( mpvClip
  ) where

import Scripts.Clipboard
import XMonad
import XMonad.Util.Run   (safeSpawn)

mpvClip :: X ()
mpvClip = liftIO mpvClip'

mpvClip' :: IO ()
mpvClip' = do
  video <- getClipboardContents
  safeSpawn "mpv" [video]
