module Colours where

import qualified Data.Map.Strict as Map
import           XMonad          (X, liftIO)
import           XResources      (getXResources)

data Colours = Colours
  { black        :: String
  , red          :: String
  , green        :: String
  , yellow       :: String
  , blue         :: String
  , purple       :: String
  , cyan         :: String
  , white        :: String
  , brightBlack  :: String
  , brightRed    :: String
  , brightGreen  :: String
  , brightYellow :: String
  , brightBlue   :: String
  , brightPurple :: String
  , brightCyan   :: String
  , brightWhite  :: String
  } deriving (Eq, Show)

data Base16 = Base16
  { base00 :: !String
  , base01 :: !String
  , base02 :: !String
  , base03 :: !String
  , base04 :: !String
  , base05 :: !String
  , base06 :: !String
  , base07 :: !String
  , base08 :: !String
  , base09 :: !String
  , base0A :: !String
  , base0B :: !String
  , base0C :: !String
  , base0D :: !String
  , base0E :: !String
  , base0F :: !String
  } deriving (Eq, Show)

tomorrowNight :: Base16
tomorrowNight = Base16
  { base00 = "1d1f21"
  , base01 = "282a2e"
  , base02 = "373b41"
  , base03 = "969896"
  , base04 = "b4b7b4"
  , base05 = "c5c8c6"
  , base06 = "e0e0e0"
  , base07 = "ffffff"
  , base08 = "cc6666"
  , base09 = "de935f"
  , base0A = "f0c674"
  , base0B = "b5bd68"
  , base0C = "8abeb7"
  , base0D = "81a2be"
  , base0E = "b294bb"
  , base0F = "a3685a"
  }

modusVivendi :: Base16
modusVivendi = Base16
  { base00 = "000000"
  , base01 = "100f10"
  , base02 = "323232"
  , base03 = "505050"
  , base04 = "e0e6f0"
  , base05 = "ffffff"
  , base06 = "e0e6f0"
  , base07 = "ffffff"
  , base08 = "ff8059"
  , base09 = "ef8b50"
  , base0A = "d0bc00"
  , base0B = "44bc44"
  , base0C = "00d3d0"
  , base0D = "2fafff"
  , base0E = "feacd0"
  , base0F = "8b1030"
  }

-- | 'getBase16Theme' @def@ gets the 'Base16' theme from the X resources, and
-- defaults to the colours of @def@ if some colour is not defined in the X
-- resources.
--
-- The colour entries must be named @colors.base00@, replacing @00@ with the
-- respective number.
getBase16Theme :: Base16 -> X Base16
getBase16Theme def = do
  resources <- liftIO getXResources
  let colour get key = Map.findWithDefault (get def) key resources
  pure $ Base16
    { base00 = colour base00 "colors.base00"
    , base01 = colour base01 "colors.base01"
    , base02 = colour base02 "colors.base02"
    , base03 = colour base03 "colors.base03"
    , base04 = colour base04 "colors.base04"
    , base05 = colour base05 "colors.base05"
    , base06 = colour base06 "colors.base06"
    , base07 = colour base07 "colors.base07"
    , base08 = colour base08 "colors.base08"
    , base09 = colour base09 "colors.base09"
    , base0A = colour base0A "colors.base0A"
    , base0B = colour base0B "colors.base0B"
    , base0C = colour base0C "colors.base0C"
    , base0D = colour base0D "colors.base0D"
    , base0E = colour base0E "colors.base0E"
    , base0F = colour base0F "colors.base0F"
    }

-- | 'withBase16Theme' @def@ @f@ runs the action @f@ in the X monad given the
-- 'Base16' theme from the X resources, defaulting to @def@, as explained in the
-- documentation of 'getBase16Theme'.
withBase16Theme :: Base16 -> (Base16 -> X a) -> X a
withBase16Theme def f = f =<< getBase16Theme def

gruvbox = Colours
  { black        = "#282828"
  , red          = "#cc241d"
  , green        = "#98971a"
  , yellow       = "#d79921"
  , blue         = "#458588"
  , purple       = "#b16286"
  , cyan         = "#689d6a"
  , white        = "#a89984"
  , brightBlack  = "#928374"
  , brightRed    = "#fb4934"
  , brightGreen  = "#b8bb26"
  , brightYellow = "#fabd2f"
  , brightBlue   = "#83a598"
  , brightPurple = "#d3869b"
  , brightCyan   = "#8ec07c"
  , brightWhite  = "#ebdbb2"
  }

spacemacsThemeDark = Colours
  { black        = "#292b2e"
  , red          = "#f2241f"
  , green        = "#67b11d"
  , yellow       = "#b1951d"
  , blue         = "#4f97d7"
  , purple       = "#a31db1"
  , cyan         = "#28def0"
  , white        = "#a89984"
  , brightBlack  = "#212026"
  , brightRed    = "#f2241f"
  , brightGreen  = "#67b11d"
  , brightYellow = "#b1951d"
  , brightBlue   = "#2d9574"
  , brightPurple = "#a31db1"
  , brightCyan   = "#28def0"
  , brightWhite  = "#a89984"
  }

greenDarkColours = Colours
  { black        = "#000000"
  , red          = "#ec6363"
  , green        = "#4adb5c"
  , yellow       = "#a6c35e"
  , blue         = "#4697db"
  , purple       = "#b883d8"
  , cyan         = "#4eb6a9"
  , white        = "#787878"
  , brightBlack  = "#111111"
  , brightRed    = "#ec6363"
  , brightGreen  = "#4adb5c"
  , brightYellow = "#a6c35e"
  , brightBlue   = "#4697db"
  , brightPurple = "#b883d8"
  , brightCyan   = "#4eb6a9"
  , brightWhite  = "#a5a5a5"
  }

base16TomorrowNightColours = Colours
  { black         = "#1d1f21"
  , red           = "#cc6666"
  , green         = "#b5bd68"
  , yellow        = "#f0c674"
  , blue          = "#81a2be"
  , purple        = "#b294bb"
  , cyan          = "#8abeb7"
  , white         = "#c5c8c6"
  , brightRed     = "#cc6666"
  , brightBlack   = "#969896"
  , brightGreen   = "#b5bd68"
  , brightYellow  = "#f0c674"
  , brightBlue    = "#81a2be"
  , brightPurple  = "#b294bb"
  , brightCyan    = "#8abeb7"
  , brightWhite   = "#ffffff"
  }
