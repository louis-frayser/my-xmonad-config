module MyViews (myWorkspaces,myExtraWorkspaces,swapCurrentViews,
  view2,helpCommand,helpWsCommand)
{- | View & View-controller definitions that must be shared between xmonad and config modules
 -}
where
--
-- Part of a modular xmonad configuration.
--

import Data.List(intercalate)
import System.IO(IOMode(ReadMode),hGetLine,hGetContents,
                  hWaitForInput,withFile,hPutStrLn,stderr)
import System.Directory(doesFileExist)
import Prelude hiding (putStrLn)
import Control.Exception(catch)
import Control.Concurrent(threadDelay)
import XMonad
import XMonad.Actions.OnScreen
-- 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
--
import Graphics.X11.ExtraTypes.XF86   -- KBD Key names
import Data.Maybe
-- import Debug.Trace

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myExtraWorkspaces = [(xK_0, "SpareA"),(xK_minus, "SpareB"),(xK_equal, "SpareC")] 
  ++
    [
      (xK_x,"Admin+") -- , (xK_KP_1,"Admin+") keypad codes don't work
    , (xK_y,"Project+")
    , (xK_z,"A/V+")
    ]


myWorkspaces    = ["Admin","Home","PIM","Practice","Research","Project","Graphics","A/V","Scratch"]
  ++ (map snd myExtraWorkspaces)

{- | Set the views (monitors) to the given workspaces -}
view2:: String->String->X ()
view2 lft rht = 
  let mov2 cs ws = windows $ greedyViewOnScreen cs ws
   in do mov2 0 lft
         mov2 1 rht

-- screenToWorkspaceId :: W.Screen (W.Workspace i j k) k2 d -> WorkspaceId
screenToWorkspaceId (W.Screen (W.Workspace wsId _ _) _sid _ ) = wsId

-- | Switch what's viewed on 1st and 2nd screens:
-- Get a focusted and an unfocused screen
-- The shift focus to the unfocused on
swapCurrentViews :: X ()
swapCurrentViews =
  withWindowSet
    (\ss  ->
        let W.Screen (W.Workspace wc_id _ _)   sc_id _ = W.current ss
            W.Screen (W.Workspace wx_id _ _) sid_x _
              = head $ W.visible ss
         in case sc_id of
                 S 0 -> view2 wx_id wc_id
                 _ -> view2 wc_id wx_id
               )
              
-- | Help
helpWsCommand :: X ()
helpWsCommand =  
   do -- verified pwd = $HOME
      -- If we don't delay until fifo is created
      -- the shell will create a regular file named "fifo"
      make_fifo
      -- menu_to_fifo: "xmesage>>fifo" blocks unless it runs
      --  after fifo is read from.
      menu_to_fifo
      result <- io get_result
      trace $ "xmonad: Switching view to `" ++ result ++ "'"
      windows $ W.greedyView result
   where
      fifodir=".xmonad/run"
      fifo=fifodir ++ "/fifo"
      make_fifo=
        do already <- io $ doesFileExist fifo
           if already then return ()
                         else do _mkfifo
                                 io $ threadDelay 1000000
      _mkfifo =                            
         spawn $ "fifodir="++fifodir ++"; fifo="++fifo++";\
               \[  -e $fifo ] || { \
               \[ -e $fifodir ] || mkdir -p $fifodir;\
               \mkfifo $fifo ;}"
      menu_to_fifo :: X ()
      menu_to_fifo = 
           do let dmbuttons=unlines myWorkspaces
              spawn $ "echo '" ++ dmbuttons ++ "'| dmenu > " ++ fifo
      -- Polls fifo for a result
      get_result = withFile fifo ReadMode getIt

     
-- | Read result from menu of workspaces
--  waitval is (-1) because a real value crashes 
getIt h =
  do catch tryit  tryit_again
       where tryit =
               do trace "getIt..."
                  ready <- hWaitForInput h (-1)
                  if ready then put_result
                     else trace "xmonad: WTF" >> return ""
             tryit_again e =
               do hPutStrLn stderr $ "E :-" ++ show (e ::IOError)
                  trace "xmonad: Waiting for input..."
                  threadDelay delayusecs
                  getIt h
             put_result =  do result <- hGetLine h
                              trace $ "xmonad: RESULT: `" ++ result ++ "'."
                              return result
             delayusecs=round $ delaysecs * 1e6
             delaysecs=0.5


helpfile = "/export/home/frayser/.xmonad/doc/xmonad.cat"
helpCommand :: X ()
helpCommand = spawn $ "xmessage -file " ++ helpfile 
{-
help:: String
help = unlines [ 
      "The default modifier key is '" ++ "L_ALT" ++ "'." 
   , "Default keybindings:"
   , "" 
   , "-- launching and killing progarms"
   , "mod-Shift-Enter Launch terminal"
   ]
-}       
-- vim: set expandtab tabstop=4 shiftwidth=4 ai:
