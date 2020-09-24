module MyKeys (myKeys,myModMask)
where
--
-- part of my XMonad configuration: 
-- the keybindings
--
import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks

import System.Exit
import System.IO (stderr, hPutStrLn)
import System.Process(runInteractiveCommand,waitForProcess)
import XMonad.Actions.OnScreen
-- 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Graphics.X11.ExtraTypes.XF86   -- KBD Key names
import MyKbFunctions(MixArg(..),alertRegularKey,alertShiftedKey,
                      amixer, lockScreen, runEmacs,screenshot,switchSession)
import MyViews(helpCommand,helpWsCommand,
               myExtraWorkspaces,
               swapCurrentViews,view2)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
--
myModMask       = mod1Mask
mod4 = mod4Mask

-- | Add a key and it's shifted counterpart for the
--   specified commands.
keycmds k cmd scmd  = 
    [ ( (modm, k),      cmd)
    , ( (modm .|. shiftMask, k), scmd)
    ]
      where modm = myModMask


-- | Display a message for Key and Shift-Key presses
debugKey k = 
    keycmds k (spawn $ "xmessage regular key: " ++ show k) (spawn  $ "xmessage shifted: "  ++ show k)


    
{-- | keyWithWS k ws
--   Add standard actions for k, shift-k for workspacs
keyWithWS k ws =
  keycmds k (windows $ W.greedyView ws) (windows $ W.shift ws)
-}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "grun")

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

    -- Lock screen
    , ((modm .|. shiftMask,  xK_l     ), lockScreen )
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
    
    -- Add shortcuts for extra workspaces
    [ ((myModMask, key), (windows $ W.greedyView ws))
       | (key,ws) <- myExtraWorkspaces
    ] ++ [
       ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
       | (key,ws) <- myExtraWorkspaces
    ]


{- | View  two workspacers as a pair -}
    ++
    [
     ((modm,  xK_a            ), view2 "Practice" "A/V")
    ,((0    , xF86XK_HomePage ), view2 "Admin"  "Admin+") 
    ,((modm , xK_F1),            view2 "Admin"  "Admin+")
    ,((0    , xF86XK_Mail),      view2 "Home" "PIM")
    ,((modm , xK_F2),            view2 "Home" "PIM")

    ,((0    , xF86XK_Search   ), view2 "PIM"    "A/V")
    ,((modm , xK_F3           ), view2 "PIM"    "A/V")

    ,((0    , xF86XK_Tools),     view2 "Practice" "Project")
    ,((modm , xK_F4),            view2 "Practice" "Project")

    ,((0    , xF86XK_Calculator),view2 "Project" "Research")
    ,((modm , xK_F11),           view2 "Graphics" "Graphics+")
    ,((modm , xK_F12),           view2 "Project" "Research")
    ]

    {- | Multimedia Keys -}
    ++
    [
     ((0   , xF86XK_AudioLowerVolume), amixer Down)
    ,((0   , xF86XK_AudioRaiseVolume), amixer Up)
    ,((0   , xF86XK_AudioMute ), amixer ToggleMute)
    ]
    ++
    
    [ -- | Swap LtoR visible workspaces
      ( (modm , xK_F5), swapCurrentViews)
    , -- | Swap Sessions
      ( (modm , xK_Super_L), switchSession )
    ]
    ++

    {- | Run Programs -}
    [
      ( (0, xK_Print), screenshot)
    , ( (modm, xK_slash),      helpWsCommand )
      -- '?' => shift-slash
    , ( (modm .|. shiftMask, xK_slash), helpCommand)
    , ( (mod4, xK_e),  runEmacs )

    {- Note colon is registered as shift-semicolon
    , ( (modm .|. shiftMask, xK_semicolon), showkey "sft-semicolon")
    , ( (modm, xK_colon), showkey "colon")
    , ( (modm, xK_semicolon), showkey "semicolon")
-}
    ] ++ debugKey xK_u
    

-- vim: set expandtab tabstop=4 shiftwidth=4 ai:
