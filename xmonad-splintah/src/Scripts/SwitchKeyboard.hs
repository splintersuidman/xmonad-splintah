module Scripts.SwitchKeyboard
  ( switchKeyboard
  ) where

import Data.Char       (isSpace)
import Data.List       (isPrefixOf)
import System.Process
import XMonad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

switchKeyboard :: X ()
switchKeyboard = liftIO switchKeyboard'

switchKeyboard' :: IO ()
switchKeyboard' = do
  query <- runProcessWithInput "setxkbmap" ["-query"] ""
  let layout = dropWhile isSpace
             . dropWhile (not . isSpace)
             . head
             . filter ("layout" `isPrefixOf`)
             . lines
             $ query
  case layout of
    "us" -> safeSpawn "setxkbmap" ["-layout", "gr", "-variant", "polytonic"]
    _    -> safeSpawn "setxkbmap" ["-layout", "us"]
