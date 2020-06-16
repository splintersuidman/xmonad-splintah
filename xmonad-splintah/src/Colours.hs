module Colours where

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
