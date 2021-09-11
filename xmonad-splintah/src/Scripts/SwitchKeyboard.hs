module Scripts.SwitchKeyboard
  ( switchKeyboard
  ) where

import Control.Category ((<<<), (>>>))
import Data.Char        (isSpace)
import Data.List        (isPrefixOf)
import XMonad
import XMonad.Util.Run  (runProcessWithInput, safeSpawn)

switchKeyboard :: X ()
switchKeyboard = liftIO switchKeyboard'

switchKeyboard' :: IO ()
switchKeyboard' = do
  query <- runProcessWithInput "setxkbmap" ["-query"] ""
  let getLayout = lines
              >>> filter ("layout" `isPrefixOf`)
              >>> head
              >>> dropWhile (not <<< isSpace)
              >>> dropWhile isSpace
  let layout = getLayout query
  case layout of
    "us" -> safeSpawn "setxkbmap" ["-layout", "gr", "-variant", "polytonic"]
    _    -> safeSpawn "setxkbmap" ["-layout", "us"]
