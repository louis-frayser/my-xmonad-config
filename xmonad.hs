--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import Data.Monoid
import System.Posix.Env
import Data.Maybe
import qualified Data.Map        as M

import System.Posix.Env(getEnv)
import System.Exit
import System.Directory

import Graphics.X11.ExtraTypes.XF86   -- KBD Key names
import XMonad.Actions.OnScreen
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Reflect
import XMonad.Layout.LayoutModifier
import XMonad
import XMonad.Util.SpawnOnce

import MyController(myKeys, myModMask, myWorkspaces)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal      = "xterm"
myTerminal      = "terminator||konsole||xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
myBorderWidth   = 1

-- myModMask defined in keys config files

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
-- See lib/MyViews.hs
--
--
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#c0c0b0"
myFocusedBorderColor = "#ffff7f"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings in the separate keys files
-- lib/MyKeys.hs
     
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (tall ||| Mirror tall ||| tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     tall    = Tall 1 (3/100) (1/2)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
-- Original was spawnOnce
myStartupHook :: String -> X ()
myStartupHook configDir = fst (spawn::String -> X (),spawnOnce) $ configDir ++ "/scripts/sanity-check.sh"

------------------------------------------------------------------------
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
type MT = Mirror Tall
type CT = Choose Tall
type ML = XMonad.Layout.LayoutModifier.ModifiedLayout
defaults :: String -> XConfig ( ML AvoidStruts (CT (Choose MT (CT (Choose MT Full)))))
defaults configDir = 
   docks 
   def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook configDir
    }

xmonad_home :: IO String
xmonad_home =
  do m_xmh <- getEnv "XMONAD_HOME"
     home <-  getEnv "HOME" >>= (\s -> return $ fromMaybe (error "No $HOME is set") s) 
     let cfg_dir=home ++ "/.config/xmonad"
     let dfl_dir=home ++ "/.xmonad" -- Old/coventional xmonad home
     is_cfg_dir <- doesDirectoryExist cfg_dir
     is_dfl_dir <- doesDirectoryExist dfl_dir
     let out = case (m_xmh,is_cfg_dir,is_dfl_dir) of
                   (Nothing, True, _) -> cfg_dir
                   (Nothing, False,True) -> dfl_dir
                   (Just xmh, _  ,  _ ) -> xmh
     return out
     

-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
-- main = xmonad defaults
main = do --  Location of the configuration
          xmhome <- xmonad_home
          putEnv $ "XMONAD_HOME=" ++ xmhome
          xmonad =<< xmobar  ( defaults  xmhome)

