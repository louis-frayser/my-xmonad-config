module MyKbFunctions
  ( MixArg(..)
  , alertRegularKey
  , alertShiftedKey
  , amixer
  , lockScreen
  , runEmacs
  , screenshot
  , showkey
  , switchSession
  , xspawn
  ) where

import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (liftIO)
import XMonad (X, io, spawn)

data Audio
  = Pulse
  | Alsa
  | Jack
  | Pipewire

-- | Local configurations
audio = Alsa

-- | Helper functions
xspawn :: String -> X ()
xspawn cmd = spawn ("PATH=/usr/lucho/bin:$PATH;" ++ cmd)

-- | PulseAudio mixer controler
pmixer :: MixArg -> X ()
{-
pmixer cmd =
  let prefix = "amixer -D pulse sset Master,0" --VV Non Pulse Below
      params =
        case cmd of
          Up -> "2000+ unmute"
          Down -> "2000- unmute"
          ToggleMute -> "0 toggle"
   in spawn $ prefix ++ " " ++ params
 -}

pmixer cmd =
  let sink = "0"
      unmute = "pactl set-sink-mute 0 0"
      cmdline =
        case cmd of
            Up         -> unmute ++ "; pactl -- set-sink-volume " ++ sink ++ " +10%"
            Down       -> unmute ++ "; pactl -- set-sink-volume " ++ sink ++ " -10%"
            ToggleMute -> "pactl set-sink-mute " ++ sink ++ " toggle"
   in do liftIO $ putStrLn cmdline
         spawn cmdline

-- | Alsa Mixer
data MixArg
  = Up
  | Down
  | ToggleMute

mixer :: MixArg -> X () -- See above for Pulse
mixer cmd =
  let cstr =
        "amixer sset Master" ++
        " " ++
        case cmd of
          Up -> "2000+ unmute"
          Down -> "2000- unmute"
          ToggleMute -> "toggle"
   in spawn cstr

-- | default mixer
amixer :: MixArg -> X ()
amixer =
  case audio of
    Alsa -> mixer
    Pulse -> pmixer

-- | Screen grab
screenshot :: X ()
screenshot = spawn "sleep 1; /usr/bin/mate-screenshot -a"

-- | Change X server via greeter
lockScreen, switchSession :: X ()
switchSession = spawn "dm-tool switch-to-greeter"

-- | Call xscreensave to lock screen and display the "logan as other user" dialog
lockScreen = spawn "xscreensaver-command -lock"

-- | Launch or refocus Emacs
runEmacs :: X ()
runEmacs = spawn "pgrep -u $USER emacs || exec emacs"

-- --------------------------------------------------------
--  ============================================================
-- | Diagonostic
-- --------------------------------------------------------
showkey :: String -> X ()
showkey k = io $ System.IO.hPutStrLn stderr $ "KEY: " ++ k

-- | Display a message for Key and Shift-Key presses
_keyAlert :: (Show k) => k -> Bool -> X ()
_keyAlert k isShifted =
  spawn $
  "xmessage " ++
  if isShifted
    then "shifted "
    else "" ++ show k

alertRegularKey, alertShiftedKey :: Show k => k -> X ()
alertRegularKey k = _keyAlert k False

alertShiftedKey k = _keyAlert k True
