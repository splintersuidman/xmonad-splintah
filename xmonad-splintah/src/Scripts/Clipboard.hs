module Scripts.Clipboard
  ( getClipboardContents
  ) where

import XMonad
import XMonad.Util.Run (runProcessWithInput)

getClipboardContents :: IO String
getClipboardContents = runProcessWithInput "xclip" ["-out"] ""
