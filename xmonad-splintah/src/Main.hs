{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Main where

import           Colours
import           Data.Char                      (isSpace)
import qualified Data.Map.Strict                as Map
import           Foreign.C.Types                (CInt (..))
import           Graphics.X11.ExtraTypes.XF86
import qualified Polybar
import           Scripts.MicLed                 (micLed)
import           Scripts.MpvClip                (mpvClip)
import           Scripts.Screenshot
import           Scripts.SwitchKeyboard         (switchKeyboard)
import           System.IO
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS         (nextWS, prevWS)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      (ewmh)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows    (boringWindows, focusDown,
                                                 focusUp)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Simplest         (Simplest (..))
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed           (Theme (..), addTabs,
                                                 shrinkText)
import           XMonad.Layout.WindowNavigation (windowNavigation)
import           XMonad.Prompt                  as Prompt
import           XMonad.Prompt.FuzzyMatch       (fuzzyMatch, fuzzySort)
import           XMonad.Prompt.Pass             (passEditPrompt,
                                                 passGeneratePrompt, passPrompt,
                                                 passRemovePrompt)
import           XMonad.Prompt.Shell            (shellPrompt)
import           XMonad.Prompt.Window           (WindowPrompt (Bring, Goto),
                                                 allWindows, windowPrompt,
                                                 wsWindows)
import           XMonad.Prompt.Workspace        (workspacePrompt)
import qualified XMonad.StackSet                as W
import           XMonad.Util.NamedScratchpad    (NamedScratchpad (NS),
                                                 namedScratchpadAction,
                                                 namedScratchpadManageHook)
import qualified XMonad.Util.NamedScratchpad    as NS
import           XMonad.Util.Run                (safeSpawn, spawnPipe)
import           XMonad.Util.Scratchpad         (scratchpadFilterOutWorkspace)

colours = base16TomorrowNightColours

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

myTheme :: Base16
myTheme = Colours.modusVivendi

withTheme :: (Base16 -> X a) -> X a
-- TODO: get this working with X resources. 'resourceManagerString' does not
-- retrieve the latest resources.
-- withTheme = withBase16Theme myTheme
withTheme f = f myTheme

myTerminal            = "urxvt -e tmux"
myTerminalNamed n     = "urxvt -title " <> n <> " -name " <> n <> " -e tmux"
myEditor              = "emacsclient --create-frame"
myFileBrowser         = "ranger"
myModMask             = mod4Mask -- Super key
myNormalBorderColour  = cBlack
myFocusedBorderColour = cBlue
myBorderWidth         = 2
myFocusFollowsMouse   = True
myFont size           = "xft:Iosevka Custom:size=" <> show size <> ":antialias=true:autohint=true"

myPromptConfig :: X XPConfig
myPromptConfig = withTheme $ \Base16 {..} ->
  pure $ def
    { font = myFont 11
    , bgColor = base00
    , fgColor = base05
    , bgHLight = base0B
    , fgHLight = base00
    , borderColor = base0D
    , alwaysHighlight = True
    , promptBorderWidth = myBorderWidth
    , position = CenteredAt (1 / 8) (3 / 4)
    , height = 30
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    , promptKeymap = Map.fromList [((myModMask, xK_space), Prompt.quit)]
                  <> vimLikeXPKeymap' (setBorderColor myNormalBorderColour) id id isSpace
                  <> emacsLikeXPKeymap
                  <> defaultXPKeymap
    }

withMyPromptConfig :: (XPConfig -> X a) -> X a
withMyPromptConfig f = f =<< myPromptConfig

myTabConfig = def
  { activeColor = cBlack
  , inactiveColor = cBlack
  , urgentColor = cRed
  , activeBorderWidth = 0
  , inactiveBorderWidth = 0
  , activeTextColor = cBlue
  , inactiveTextColor = cWhite
  , urgentTextColor = cBlack
  , fontName = myFont 11
  }

myWorkspaces = fmap show [1..9 :: Int]

myScratchpads =
  [ NS
    { NS.name = "scratchpad"
    , NS.cmd = myTerminalNamed "scratchpad"
    , NS.query = title =? "scratchpad"
    , NS.hook = NS.customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
    }
  , NS
    { NS.name = "agenda"
    , NS.cmd = "emacsclient --create-frame --frame-parameters '((name . \"agenda\"))' --eval '(progn (org-agenda nil \"n\") (delete-other-windows))'"
    , NS.query = title =? "agenda"
    , NS.hook = NS.customFloating $ W.RationalRect (1/16) (1/16) (7/8) (7/8)
    }
  ]

-- | Move the mouse pointer to the centre of the window.
mouseToWindowCentre :: Display -> Window -> X ()
mouseToWindowCentre display window = liftIO $ do
  WindowAttributes {wa_width = width, wa_height = height} <-
    getWindowAttributes display window
  -- 'warpPointer' moves the mouse pointer relative to the origin of the
  -- destination window (the third argument, here 'window'). The x and y
  -- coordinates of the focused window are thus not needed to move the mouse
  -- pointer to the centre of the window.
  let CInt x = width  `div` 2
  let CInt y = height `div` 2
  warpPointer display 0 window 0 0 0 0 x y

-- | Move the mouse pointer to the centre of the focused window.
mouseFollowsFocus :: X ()
mouseFollowsFocus =
  withFocused $ \window ->
  withDisplay $ \display ->
  mouseToWindowCentre display window

myKeys conf@XConfig {XMonad.modMask = modm} = Map.fromList $
  ---- Applications
  -- Launch terminal
  [ ((modm, xK_Return), spawn myTerminal)
  -- Run editor
  , ((modm, xK_e), spawn myEditor)
  -- Run file browser
  , ((modm, xK_f), spawn $ myTerminalNamed "ranger" <> " -c " <> myFileBrowser)
  -- Run mpv with clipboard contents
  , ((modm, xK_v), mpvClip)
  -- Screenshot
  , ((0, xK_Print), withMyPromptConfig screenshotPrompt)

  -- Switch keyboard layout.
  , ((modm, xK_slash), switchKeyboard)
  -- Toggle bar.
  , ((modm, xK_b), sendMessage ToggleStruts)

  ---- xmonad
  -- Kill focused window
  , ((modm, xK_q), kill)
  , ((modm .|. shiftMask, xK_c), kill)
  -- Next layout
  , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
  -- Reset layout to default
  -- , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  -- Resize viewed windows to correct size
  , ((modm, xK_n), refresh)
  -- Focus next window
  , ((modm, xK_j), focusDown >> mouseFollowsFocus)
  -- Focus previous window
  , ((modm, xK_k), focusUp >> mouseFollowsFocus)
  -- Move mouse pointer to centre of focused window
  , ((modm, xK_i), mouseFollowsFocus)
  -- Focus master
  -- , ((modm, xK_m), windows W.focusMaster)
  -- Swap focused window with master
  -- , ((modm, xK_Return), windows W.swapMaster)
  -- Swap focused window with below window
  , ((modm .|. shiftMask, xK_j), windows W.swapDown)
  -- Swap focused window with above window
  , ((modm .|. shiftMask, xK_k), windows W.swapUp)
  -- Previous workspace
  , ((modm, xK_h), prevWS)
  -- Next workspace
  , ((modm, xK_l), nextWS)
  -- Shrink master area
  , ((modm .|. shiftMask, xK_h), sendMessage Shrink)
  -- Expand master area
  , ((modm .|. shiftMask, xK_l), sendMessage Expand)
  -- Tile window
  , ((modm, xK_t), withFocused $ windows . W.sink)
  -- Increment the number of windows in the master area
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  -- Decrement the number of windows in the master area
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  -- Restart xmonad
  , ((modm .|. shiftMask, xK_q), recompile True >> restart "xmonad" True)
  -- Spawn terminal scratchpad.
  , ((modm, xK_s), namedScratchpadAction myScratchpads "scratchpad")
  -- Spawn agenda scratchpad.
  , ((modm, xK_a), namedScratchpadAction myScratchpads "agenda")

  ---- Sublayouts
  , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
  , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
  , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
  , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
  , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
  , ((modm, xK_Tab), onGroup W.focusDown')
  , ((modm .|. shiftMask, xK_Tab), onGroup W.focusUp')

  ---- Prompt
  -- Shell prompt
  , ((modm, xK_space), withMyPromptConfig shellPrompt)
  -- Go to workspace
  , ((modm, xK_m), withMyPromptConfig $ \xpconf -> workspacePrompt xpconf (windows . W.greedyView))
  -- Move window to workspace
  , ((modm .|. shiftMask, xK_m), withMyPromptConfig $ \xpconf -> workspacePrompt xpconf (windows . W.shift))
  -- Go to window on workspace
  , ((modm, xK_w), withMyPromptConfig $ \xpconf -> windowPrompt xpconf Goto wsWindows)
  -- Go to window
  , ((modm .|. controlMask, xK_w), withMyPromptConfig $ \xpconf -> windowPrompt xpconf Goto allWindows)
  -- Bring window
  , ((modm .|. shiftMask, xK_w), withMyPromptConfig $ \xpconf -> windowPrompt xpconf Bring allWindows)
  -- Pass
  , ((modm , xK_p), withMyPromptConfig passPrompt)
  -- Pass generate
  , ((modm .|. shiftMask, xK_p), withMyPromptConfig passGeneratePrompt)
  -- Pass edit
  , ((modm .|. controlMask, xK_p), withMyPromptConfig passEditPrompt)
  -- Pass remove
  , ((modm .|. controlMask .|. shiftMask, xK_p), withMyPromptConfig passRemovePrompt)

  ---- Audio and music
  -- Play/pause
  , ((0, xF86XK_Launch1), spawn "mpc toggle")
  , ((0, xF86XK_AudioPlay), spawn "mpc toggle")
  , ((0, xK_Pause), spawn "mpc toggle")
  , ((modm, xK_F7), spawn "mpc toggle")
  -- Pause
  , ((0, xF86XK_AudioStop), spawn "mpc stop")
  , ((modm, xK_F9), spawn "mpc stop")
  -- Next track
  , ((0, xF86XK_AudioNext), spawn "mpc next")
  , ((modm, xK_F8), spawn "mpc next")
  -- Previous track
  , ((0, xF86XK_AudioPrev), spawn "mpc prev")
  , ((modm, xK_F6), spawn "mpc prev")
  -- Lower volume
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
  , ((modm, xK_Page_Down), spawn "amixer set Master 5%-")
  , ((modm, xK_F11), spawn "amixer set Master 5%-")
  -- Raise volume
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
  , ((modm, xK_Page_Up), spawn "amixer set Master 5%+")
  , ((modm, xK_F12), spawn "amixer set Master 5%+")
  -- Toggle audio
  , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")
  , ((modm, xK_F10), spawn "amixer set Master toggle")
  -- Toggle microphone
  , ((0, xF86XK_AudioMicMute), micLed)

  ---- Brightness
  -- Lower brightness
  , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl --device=amdgpu_bl0 set +5")
  -- Raise brightness
  , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl --device=amdgpu_bl0 set 5-")
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
  windowNavigation $
  boringWindows $
  addTabs shrinkText myTabConfig . subLayout [] Simplest $
  spacingRaw True (Border 2 2 2 2) True (Border 2 2 2 2) True $
  avoidStruts $
  tiled ||| Mirror tiled ||| noBorders Full ||| simplestFloat
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

myManageHook =
  namedScratchpadManageHook myScratchpads <> manageDocks

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

polybarLogHook :: X ()
polybarLogHook = -- withTheme $ \Base16 {..} ->
  dynamicLogWithPP (Polybar.defPolybarPP "/tmp/.xmonad-log")
    { ppTitle = const ""
    , ppCurrent = Polybar.underline base0D . Polybar.color base00 base0D . pad
    , ppHidden = Polybar.foreground base05 . pad
    , ppVisible = Polybar.foreground base05 . pad
    , ppUrgent = Polybar.underline base08 . Polybar.foreground base05 . pad
    , ppLayout = const ""
    , ppSep = ""
    , ppWsSep = ""
    , ppSort = (scratchpadFilterOutWorkspace .) <$> ppSort def
    }
  where
    Base16 {..} = myTheme

myLogHook = polybarLogHook

myStartupHook = docksStartupHook <+> setWMName "LG3D"

main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-log"]

  xmonad . ewmh . fullscreenSupport $ def
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
