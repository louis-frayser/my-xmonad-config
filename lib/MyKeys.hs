module MyKeys (myKeys,myModMask)
where
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
-- import XMonad.Util.Run(runProcessWithInput,runProcessWithInputAndWait)

import System.Exit
import System.IO (stderr, hPutStrLn)
import System.Process(runInteractiveCommand,waitForProcess)
import XMonad.Actions.OnScreen
-- 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Graphics.X11.ExtraTypes.XF86   -- KBD Key names
import MyViews

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
--
myModMask       = mod1Mask

-- | Helper functions
xspawn :: String -> X ()
xspawn cmd =spawn ("PATH=/usr/lucho/bin:$PATH;" ++ cmd)

screenshot::  X()
screenshot=spawn "sleep 1; /usr/bin/mate-screenshot -a"

-- | Alsa Mixer
data MixArg = Up | Down | ToggleMute
amixer :: MixArg -> X ()
amixer cmd =
  let cstr = 
         "amixer sset " ++ 
            case cmd of 
              Up   -> "PCM playback 20+"
              Down -> "PCM playback 20-"
              ToggleMute -> "Master toggle"
   in spawn cstr
   

-- | Multimedia  (Pulse Audio)
volToggle,volDn,volUp ::   X ()
volDn = spawn "amixer -D pulse sset Master,0 2000- unmute"
volUp = spawn "amixer -D pulse sset Master,0 2000+ unmute"
volToggle = spawn "amixer -D pulse sset Master,0 toggle"


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), xspawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), xspawn "grun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    
    ++
    
    -- Add shortcuts for exra workspaces
    [ ((myModMask, key), (windows $ W.greedyView ws))
       | (key,ws) <- myExtraWorkspaces
    ] ++ [
       ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
       | (key,ws) <- myExtraWorkspaces
    ]


{- | View  two workspacers as a pair -}
    ++
    [
     ((modm,  xK_a            ), view2 "A/V+" "Graphics+")
    ,((0    , xF86XK_HomePage ), view2 "Admin"  "Admin+") 
    ,((modm , xK_F1),            view2 "Admin"  "Admin+")
    ,((0    , xF86XK_Search   ), view2 "PIM"    "A/V")
    ,((modm , xK_F3           ), view2 "PIM"    "A/V")
    ,((0    , xF86XK_Calculator),view2 "Project" "Research")
    ,((modm , xK_F12),           view2 "Project" "Research")
    ,((0    , xF86XK_Mail),      view2 "Home" "PIM")
    ,((modm , xK_F2),            view2 "Home" "PIM")
    ,((0    , xF86XK_Tools),     view2 "Practice" "Project")
    ,((modm , xK_F4),            view2 "Practice" "Project")
    ]

{- | Multimedia Keys -}
    ++
    [
     ((0   , xF86XK_AudioLowerVolume), amixer Down)
    ,((0   , xF86XK_AudioRaiseVolume), amixer Up)
    ,((0   , xF86XK_AudioMute ), amixer ToggleMute)
    ]

{- | Swap ltor visible workspaces -}

  ++
  [
    ( (modm , xK_F5), swapCurrentViews)
  ]
  -- 
-- | Run Program
    ++
    [
      ( (0, xK_Print), screenshot)
    , ( (modm, xK_slash),      helpWsCommand)
    , ( (modm .|. shiftMask, xK_slash), helpCommand)
--    , ( (modm, xK_question), helpCommand)

{- Note colon is registered as shift-semicolon
    , ( (modm .|. shiftMask, xK_semicolon), showkey "sft-semicolon")
    , ( (modm, xK_colon),          showkey "colon")

    , ( (modm, xK_semicolon), showkey "semicolon")

    
--  , ( (modm, xK_Home), screenshot)
-}
    ]

    ++ kcmds xK_u (spawn $ "xmessage regular key: " ++ show xK_u) (spawn  $ "xmessage shifted: "  ++ show xK_u)

kcmds k cmd scmd  = 
    [ ( (modm, k),      cmd)
    , ( (modm .|. shiftMask, k), scmd)
    ]
       where modm = myModMask

keyWithWS k ws = kcmds k (windows $ W.greedyView ws) 
                         (windows $ W.shift ws)

showkey :: String -> X ()
showkey k = io $ System.IO.hPutStrLn stderr $  "KEY: " ++  k
-- vim: set expandtab tabstop=4 shiftwidth=4 ai:
