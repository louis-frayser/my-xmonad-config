module MyKbFunctions (MixArg(..),
                      alertRegularKey, alertShiftedKey,
                       amixer,
                      lockScreen,
                       screenshot,
                       showkey,
                       switchSession, xspawn)
where
import System.IO(hPutStrLn,stderr)
import XMonad(X,io,spawn)
data Audio = Pulse | Alsa | Jack

-- | Local configurations
audio = Alsa


-- | Helper functions
xspawn :: String -> X ()
xspawn cmd =spawn ("PATH=/usr/lucho/bin:$PATH;" ++ cmd)

-- | PulseAudio mixer controler
pmixer :: MixArg -> X ()
pmixer cmd =
  let prefix = "amixer -D pulse sset Master,0"
      params = case cmd of
                 Up         -> "2000- unmute"
                 Down       -> "2000- unmute"
                 ToggleMute -> "0 toggle"
  in 
    spawn $ prefix ++ " " ++ params


-- | Alsa Mixer
data MixArg = Up | Down | ToggleMute
mixer :: MixArg -> X ()
mixer cmd =
  let cstr = "amixer sset " ++ 
            case cmd of 
              Up   -> " Master 100%; " ++ "amixer sset PCM playback 20+"
              Down -> "PCM playback 20-"
              ToggleMute -> "Master toggle"
   in spawn cstr

-- | default mixer
amixer :: MixArg -> X ()
amixer = case audio of Alsa  -> mixer
                       Pulse -> pmixer

-- | Screen grab
screenshot::  X()
screenshot=spawn "sleep 1; /usr/bin/mate-screenshot -a"

-- | Change X server via greeter
lockScreen,switchSession ::  X()
switchSession = spawn "dm-tool switch-to-greeter"

-- | Call xscreensave to lock screen and display the "logan as other user" dialog
lockScreen = spawn "xscreensaver-command -lock"

-- --------------------------------------------------------
--  ============================================================
-- | Diagonostic
-- --------------------------------------------------------
showkey :: String -> X ()
showkey k = io $ System.IO.hPutStrLn stderr $  "KEY: " ++  k


-- | Display a message for Key and Shift-Key presses

_keyAlert :: (Show k) => k -> Bool -> X ()
_keyAlert k isShifted =
  spawn $ "xmessage " ++
  if isShifted
     then "shifted " else ""  ++ show k 

alertRegularKey,alertShiftedKey :: Show k => k -> X ()
alertRegularKey k = _keyAlert k False
alertShiftedKey k = _keyAlert k True



