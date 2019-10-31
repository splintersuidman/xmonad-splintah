{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main where

import qualified Data.Map                     as Map
import           Foreign
import           Foreign.C.Types
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xlib.Misc
import qualified Polybar
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.NoBorders
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet              as W

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
import           XMonad.Util.Run              (safeSpawn, spawnPipe)

colours = gruvbox

cBlack        = black colours
cRed          = red colours
cGreen        = green colours
cYellow       = yellow colours
cBlue         = blue colours
cPurple       = purple colours
cCyan         = cyan colours
cWhite        = white colours
cBrightBlack  = brightBlack colours
cBrightRed    = brightRed colours
cBrightGreen  = brightGreen colours
cBrightYellow = brightYellow colours
cBrightBlue   = brightBlue colours
cBrightPurple = brightPurple colours
cBrightCyan   = brightCyan colours
cBrightWhite  = brightWhite colours

dmenuOptions = unwords
  ["-fn", "Iosevka:size=9"
  , "-i"
  , "-nb", wrap "'" "'" cBlack
  , "-nf", wrap "'" "'" cWhite
  , "-sb", wrap "'" "'" cBlue
  , "-sf", wrap "'" "'" cBrightWhite
  ]

myTerminal            = "urxvt -e screen"
myMusicPlayer         = "ncmpcpp"
myFileBrowser         = "ranger"
myModMask             = mod4Mask -- Windows key
myNormalBorderColour  = cBrightBlack
myFocusedBorderColour = cBlue
myBorderWidth         = 3
myFocusFollowsMouse   = True

myWorkspaces = fmap show [1..9 :: Int]

-- | Move the mouse pointer to the centre of the window.
mouseToWindowCentre :: Display -> Window -> X ()
mouseToWindowCentre display window = liftIO $ do
  WindowAttributes {wa_width = width, wa_height = height} <-
    getWindowAttributes display window
  -- 'warpPointer' moves the mouse pointer relative to the origin of the
  -- destination window (the third argument, here 'window'). The x and y
  -- coordinates of the focused window are thus not needed to move the mouse
  -- pointer to the centre of the window.
  let x = unCInt (width  `div` 2)
  let y = unCInt (height `div` 2)
  warpPointer display 0 window 0 0 0 0 x y

-- | Move the mouse pointer to the centre of the focused window.
mouseFollowsFocus :: X ()
mouseFollowsFocus =
  withFocused $ \window ->
  withDisplay $ \display ->
  mouseToWindowCentre display window

-- | Removes the 'CInt' constructor.
unCInt (CInt i) = i

myKeys conf@XConfig {XMonad.modMask = modm} = Map.fromList $
  ---- Applications
  -- Launch terminal
  [ ((modm, xK_Return), spawn myTerminal)
  -- Launch Emacs.
  , ((modm, xK_e), spawn "emacs")
  -- Run dmenu
  , ((modm, xK_space), spawn $ "dmenu_run " <> dmenuOptions)
  -- Run passmenu
  , ((modm, xK_p), spawn $ "~/scripts/passmenu " <> dmenuOptions)
  -- Run file browser
  , ((modm, xK_f), spawn $ myTerminal <> " " <> myFileBrowser)
  -- Run music player
  , ((modm, xK_m), spawn $ myTerminal <> " " <> myMusicPlayer)
  -- Run mpv with clipboard contents
  , ((modm, xK_v), spawn "~/scripts/mpvclip")

  -- Switch keyboard layout.
  , ((modm, xK_slash), spawn "~/scripts/switch-keyboard.sh")

  ---- xmonad
  -- Kill focused window
  , ((modm .|. shiftMask, xK_c), kill)
  -- Next layout
  , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
  -- Reset layout to default
  -- , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to correct size
  , ((modm, xK_n), refresh)
  -- Focus next window
  , ((modm, xK_j), windows W.focusDown >> mouseFollowsFocus)
  -- Focus previous window
  , ((modm, xK_k), windows W.focusUp >> mouseFollowsFocus)
  -- Move mouse pointer to cetre of focused window
  , ((modm, xK_i), mouseFollowsFocus)
  -- Focus master
  -- , ((modm, xK_m), windows W.focusMaster)
  -- Swap focused window with master
  -- , ((modm, xK_Return), windows W.swapMaster)
  -- Swap focused window with below window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  -- Swap focused window with above window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  -- Shrink master area
  , ((modm, xK_h), sendMessage Shrink)
  -- Expand master area
  , ((modm, xK_l), sendMessage Expand)
  -- Tile window
  , ((modm, xK_t), withFocused $ windows . W.sink)
  -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  -- Decrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  -- Quit xmonad
  -- , ((m .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  -- Restart xmonad
  , ((modm, xK_q), spawn "notify-send 'Recompiling xmonad...'; xmonad --recompile; xmonad --restart")

  ---- Audio and music
  -- Play/pause
  , ((0, xF86XK_Launch1), spawn "mpc toggle")
  , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
  -- Pause
  , ((0, xF86XK_AudioStop), spawn "mpc pause")
  -- Next track
  , ((0, xF86XK_AudioNext), spawn "mpc next")
  -- Previous track
  , ((0, xF86XK_AudioPrev), spawn "mpc prev")
  -- Lower volume
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-")
  -- Raise volume
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+")
  ]
  <>
  [ ((modm .|. m, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_1..xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
  ]
  <>
  [ ((modm .|. shiftMask, xK_0), windows copyToAll)
  , ((modm .|. shiftMask .|. controlMask, xK_0), windows copyToAll)
  ]
  -- NOTE: for multi-monitor support, keys can be added here.

myMouseBindings XConfig {XMonad.modMask = modm} = Map.fromList
  -- Float window and move by dragging
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  -- Raise window to top of stack
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  -- Float window and resize by dragging
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  ]

myLayoutHook =
  spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $
  avoidStruts $
  tiled ||| Mirror tiled ||| simplestFloat ||| noBorders Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook = manageDocks <+> composeAll
  [ className =? "Gimp"        --> doFloat
  , className =? "mpv"         --> doFloat
  , className =? "Tor Browser" --> doFloat
  ]

myHandleEventHook = docksEventHook

xmobarLogHook xmobarProc = dynamicLogWithPP xmobarPP
  { ppOutput  = hPutStrLn xmobarProc
  , ppTitle   = xmobarColor cBrightWhite cBlue . pad . shorten 75
  , ppCurrent = xmobarColor cBrightWhite cBlue . pad
  , ppHidden  = xmobarColor cWhite cBlack . pad
  , ppVisible = xmobarColor cWhite cBlack . pad
  , ppUrgent  = xmobarColor cBlack cWhite . pad
  , ppLayout  = xmobarColor cWhite cBlack . pad . (":: " <>)
  , ppSep     = ""
  , ppWsSep   = ""
  }

polybarLogHook = dynamicLogWithPP (Polybar.defPolybarPP "/tmp/.xmonad-log")
  { ppTitle = Polybar.color cBrightWhite cBlue . pad . shorten 75
  , ppCurrent = Polybar.underline cBrightBlue . Polybar.color cBrightWhite cBlue . pad
  , ppHidden = Polybar.color cWhite cBlack . pad
  , ppVisible = Polybar.color cWhite cBlack . pad
  , ppUrgent = Polybar.underline cRed . Polybar.color cWhite cBlack . pad
  , ppLayout = const " "
  -- , ppLayout = Polybar.color cWhite cBlack . pad . (":: " <>)
  , ppSep = ""
  , ppWsSep = ""
  }

myLogHook = polybarLogHook

myStartupHook = setWMName "LG3D"

main = do
  -- xmobarProc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
  spawn "~/.config/polybar/launch.sh"
  safeSpawn "mkfifo" ["/tmp/.xmonad-log"]

  xmonad $ def
    { modMask            = myModMask
    , normalBorderColor  = myNormalBorderColour
    , focusedBorderColor = myFocusedBorderColour
    , borderWidth        = myBorderWidth
    , terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , workspaces         = myWorkspaces
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook
    , handleEventHook    = myHandleEventHook
    , startupHook        = myStartupHook
    }
