{-# LANGUAGE NamedFieldPuns #-}

module Mpris
  ( -- $info
    -- * Usage
    -- $usage
    mprisCreateProcess
  , mprisToggle
  , mprisPlay
  , mprisPause
  , mprisStop
  , mprisPlayAll
  , mprisPauseAll
  , mprisStopAll

  , mprisPrompt
  ) where

import           Control.Monad               (unless, when)
import           Data.Bifunctor              (first, second)
import           Data.Foldable               (for_, traverse_)
import           Data.Maybe                  (isJust)
import           System.IO
import           System.Process
import           XMonad
import           XMonad.Config.Prime         (ExtensionClass (..))
import           XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Run             (runProcessWithInput, safeSpawn)

-- $info
--
-- This module adds support for controlling media playback using the
-- MPRIS interface. To use it, you need to have the @playerctl@
-- program in your @PATH@.
--
-- TODO: add support for next and previous track actions.

-- $usage
--
-- Add to your XMonad configuration:
--
-- > import MPRIS
--
-- Add 'mprisCreateProcess' to your startup hook:
--
-- > myStartupHook = mprisCreateProcess <+> ...
--
-- Add to your keybindings something like:
--
-- >   , ((0, xF86XK_AudioPlay), mprisToggle)
-- >   , ((0, xF86XK_AudioStop), mprisStopAll)
--
-- To use the prompt, you can add to your keybindings something like:
--
-- >   , ((modm, xF86XK_AudioPlay), mprisPrompt def)

--------------------------------------------------------------------------------
-- Player control --------------------------------------------------------------

-- | The state for handling Mpris commands.
data MprisState = MprisState
  { lastPlayer :: !String
    -- ^ The last active player.
  , process    :: Maybe (Handle, ProcessHandle)
    -- ^ The playerctl process. The first value is the handle to the
    -- output of the process, the second is the process handle.
    --
    -- This value is wrapped in a 'Maybe', because the process should
    -- be created inside the 'X' monad, and can't be created by
    -- 'initialValue' of 'ExtensionClass'.
  }

instance ExtensionClass MprisState where
  initialValue = MprisState
    { lastPlayer = ""
    , process    = Nothing
    }

-- | Creates the playerctl process if there is no process running.
mprisCreateProcess :: X ()
mprisCreateProcess = do
  MprisState { process } <- XS.get
  createProcess <-
    case process of
      -- Create a process if no process was started.
      Nothing          -> pure True
      -- Create a process is the process was terminated.
      Just (_, handle) -> liftIO $ processIsTerminated handle

  when createProcess $ do
    (_, outputHandle, _, processHandle) <- liftIO $
      runInteractiveProcess
        "playerctl"
        ["--follow", "--all-players", "--format", "{{playerName}} {{status}}", "status"]
        Nothing
        Nothing
    XS.modify $ \state ->
      state { process = Just (outputHandle, processHandle) }

-- | Return whether the process was terminated.
processIsTerminated :: ProcessHandle -> IO Bool
processIsTerminated handle = isJust <$> getProcessExitCode handle

-- | The plackback status as per
-- https://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html#Property:PlaybackStatus.
--
-- The 'Read' and 'Show' instances conform to the specification.
data PlaybackStatus
  = Playing
  | Paused
  | Stopped
  deriving (Read, Show, Eq)

-- | Run an action with the playerctl handles as its arguments.
withMprisProcess :: (Handle -> ProcessHandle -> X ()) -> X ()
withMprisProcess = withMprisProcess' ()

-- | Run an action à la 'withMprisProcess' with a default argument
-- instead of '()'.
withMprisProcess' :: a -- ^ Default value.
                  -> (Handle -> ProcessHandle -> X a)
                  -> X a
withMprisProcess' def m = do
  MprisState { process } <- XS.get
  case process of
    Nothing                            -> pure def
    Just (outputHandle, processHandle) -> m outputHandle processHandle

-- | @fetchMprisEvent@ tries to read and parse a line from output of
-- the playerctl process. If there is no line in the output or if
-- there is no process, the value will be 'Nothing'.
fetchMprisEvent :: X (Maybe (String, PlaybackStatus))
fetchMprisEvent = withMprisProcess' Nothing $ \output _ -> do
  ready <- liftIO $ hReady output
  if ready
    then do
      line <- liftIO $ hGetLine output
      pure . Just . second read . splitOn (== ' ') $ line
    else pure Nothing

-- | Example: @splitOn (== ' ') "foo bar" == ("foo", "bar")@.
splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn p = second tail . break p

-- | Fetch as many events as possible (i.e., until @fetchMprisEvent@
-- returns 'Nothing').
fetchMprisEvents :: X [(String, PlaybackStatus)]
fetchMprisEvents = do
  event <- fetchMprisEvent
  case event of
    Nothing -> pure []
    Just e  -> (e :) <$> fetchMprisEvents

-- | Handle an event.
handleMprisEvent :: (String, PlaybackStatus) -> X ()
handleMprisEvent (player, status) = case status of
  Paused  -> setLastPlayer
  Playing -> setLastPlayer
  _       -> pure ()
  where
    setLastPlayer = XS.modify $ \state ->
      state { lastPlayer = player}

-- | Handle all events.
handleMprisEvents :: X ()
handleMprisEvents = do
  events <- fetchMprisEvents
  traverse_ handleMprisEvent events

-- | @mprisPlay p@ plays player @p@.
mprisPlay :: String -> X ()
mprisPlay p = mprisPlayer p "play"

-- | @mprisPause p@ pauses player @p@.
mprisPause :: String -> X ()
mprisPause p = mprisPlayer p "pause"

-- | @mprisStop p@ stops player @p@.
mprisStop :: String -> X ()
mprisStop p = mprisPlayer p "stop"

-- | @mprisPlayer p a@ runs action @a@ on player @p@.
mprisPlayer :: String -> String -> X ()
mprisPlayer p a = safeSpawn "playerctl" ["--player", p, a]

-- | Play all players.
mprisPlayAll :: X ()
mprisPlayAll = mprisAll "play"

-- | Pause all players.
mprisPauseAll :: X ()
mprisPauseAll = mprisAll "pause"

-- | Stop all players.
mprisStopAll :: X ()
mprisStopAll = mprisAll "stop"

-- | @mprisAll a@ runs action @a@ on all players.
mprisAll :: String -> X ()
mprisAll action = safeSpawn "playerctl" ["--all-players", action]

-- | Play the last active player, if there is one (that means that
-- this will do nothing unless a player was paused after XMonad
-- start-up.).
mprisPlayLastPlayer :: X ()
mprisPlayLastPlayer = do
  handleMprisEvents
  MprisState { lastPlayer } <- XS.get
  unless (null lastPlayer) $
    safeSpawn "playerctl" ["--player", lastPlayer, "play"]

-- | If there are playing players, pause them all, otherwise play the
-- last active player.
mprisToggle :: X ()
mprisToggle = do
  status <- mprisStatus
  if any ((== Playing) . snd) status
    then mprisPauseAll
    else mprisPlayLastPlayer

-- | Return the status of all players.
mprisStatus :: X [(String, PlaybackStatus)]
mprisStatus = liftIO mprisStatusIO

mprisStatusIO :: IO [(String, PlaybackStatus)]
mprisStatusIO = do
  -- Use runProcessWithInput with an empty input, because it returns
  -- the output of the process.
  status <- runProcessWithInput
    "playerctl"
    [ "--all-players"
    , "--format"
    , "{{playerName}} {{status}}"
    , "status"
    ]
    ""
  let players = fmap (second read . splitOn (== ' ')) . lines $ status
  pure players

--------------------------------------------------------------------------------
-- Prompt ----------------------------------------------------------------------

data MprisPrompt = MprisPrompt

instance XPrompt MprisPrompt where
  showXPrompt _ = "Toggle playing: "

type Predicate = String -> String -> Bool

getMprisCompl :: [String] -> Predicate -> String -> IO [String]
getMprisCompl compls p s = pure $ filter (p s) compls

-- | The MPRIS prompt. The selected option will be toggled; paused and
-- stopped players will be set to playing, playing players will be
-- paused.
mprisPrompt :: XPConfig -> X ()
mprisPrompt c = do
  status <- mprisStatus
  let players = fmap showOption status
  mkXPrompt MprisPrompt c (getMprisCompl players $ searchPredicate c) $ \option -> do
    let (player, status) = readOption option
    case status of
      Playing -> mprisPause player
      Paused  -> mprisPlay player
      Stopped -> mprisPlay player
  where
    -- For showOption and readOption the following laws should hold
    -- for valid inputs:
    -- - showOption . readOption == id
    -- - readOption . showOption == id

    showOption :: (String, PlaybackStatus) -> String
    showOption (player, status) = player <> " (" <> show status <> ")"

    readOption :: String -> (String, PlaybackStatus)
    readOption = second (read . unwrapParens) . splitOn (== ' ')
      where
        unwrapParens = takeWhile (/= ')') . dropWhile (== '(')
