{-# LANGUAGE LambdaCase #-}

module Scripts.Screenshot
  ( screenshotPrompt
  ) where

import Text.Read       (readMaybe)
import XMonad
import XMonad.Prompt   (XPConfig (..), XPrompt (..), mkXPrompt)
import XMonad.Util.Run (safeSpawn)

data ScreenshotPrompt = ScreenshotPrompt

instance XPrompt ScreenshotPrompt where
  showXPrompt _ = ""

type Predicate = String -> String -> Bool

getCompl :: [String] -> Predicate -> String -> IO [String]
getCompl compls p s = pure $ filter (p s) compls

data ScreenshotOption
  = FullScreen
  | FocusedWindow
  | FocusedWindowWithBorder
  | SelectWindowOrRectangle
  deriving (Enum, Bounded)

instance Show ScreenshotOption where
  show = \case
    FullScreen -> "Full screen"
    FocusedWindow -> "Focused window"
    FocusedWindowWithBorder -> "Focused window with border"
    SelectWindowOrRectangle -> "Select window or rectangle"

instance Read ScreenshotOption where
  readsPrec _
     =  (FullScreen <$ string (show FullScreen))
    <|> (FocusedWindow <$ string (show FocusedWindow))
    <|> (FocusedWindowWithBorder <$ string (show FocusedWindowWithBorder))
    <|> (SelectWindowOrRectangle <$ string (show SelectWindowOrRectangle))
   where
    mapReadS :: (a -> b) -> ReadS a -> ReadS b
    mapReadS f r = fmap (\(x, xs) -> (f x, xs)) . r

    (<$) :: b -> ReadS a -> ReadS b
    (<$) = mapReadS . const

    (<|>) :: ReadS a -> ReadS a -> ReadS a
    r <|> s = \xs -> r xs ++ s xs

    string :: String -> ReadS String
    string []     ys     = [("", ys)]
    string (x:xs) (y:ys)
      | x == y           = mapReadS (y :) (string xs) ys
      | otherwise        = []

screenshotPrompt :: XPConfig -> X ()
screenshotPrompt c =
  mkXPrompt ScreenshotPrompt c (getCompl options $ searchPredicate c) $ \option ->
    case readMaybe option of
      Just FullScreen -> safeSpawn "scrot" []
      Just FocusedWindow -> safeSpawn "scrot" ["--focused"]
      Just FocusedWindowWithBorder -> safeSpawn "scrot" ["--focused", "--border"]
      Just SelectWindowOrRectangle -> safeSpawn "scrot" ["--select"]
      Nothing -> pure ()
 where
  options = fmap show $ [minBound..maxBound :: ScreenshotOption]
