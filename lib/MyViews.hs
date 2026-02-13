module MyViews
  ( myWorkspacesL
  , myWorkspacesR
  , myWorkspaces
  , myDesktops
  , swapCurrentViews
  , xdpyToggle
  , view2
  , viewDesktop
  , helpCommand
  , helpWsCommand
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (liftM)
import Text.Printf

{- | View & view-controller definitions that must be shared between xmonad and config modules
 -}
--
-- Part of a modular xmonad configuration.
--
import Data.List (intercalate)
import Prelude hiding (putStrLn)
import System.Directory (doesFileExist)
import System.IO
  ( IOMode(ReadMode)
  , hGetContents
  , hGetLine
  , hPutStrLn
  , hWaitForInput
  , stderr
  , withFile
  )
import System.Posix.Env (getEnv)
import XMonad
import XMonad.Actions.OnScreen
import XMonad.Actions.Navigation2D

import qualified Data.Map as M

-- 
import qualified XMonad.StackSet as W

import Data.Maybe
--
import Graphics.X11.ExtraTypes.XF86 -- KBD Key names

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
myWorkspacesL =
  [ "Tmp"
  , "Admin"
  , "Home"
  , "PIM"
  , "Practice"
  , "Research"
  , "Project"  -- 10th saved in !! 0
  , "Graphics"
  , "A/V"
  , "Scratch"
  , "Eleven"
  , "Twelve"
  ]

myWorkspacesR = map (++ "+") myWorkspacesL

myWorkspaces = myWorkspacesL ++ myWorkspacesR
myDesktops = zip myWorkspacesL myWorkspacesR -- pairs of WS
desktop n = myDesktops !! (n - 1)  -- select a desktop

{- | Set the views (monitors) to the given workspaces -}
view2 :: String -> String -> X ()
view2 lft rht =
  let mov2 cs ws = windows $ greedyViewOnScreen cs ws
   in do mov2 0 lft
         mov2 1 rht
-- --------------------------------------------------------------
viewDesktop :: String -> X ()
viewDesktop indicator =
  case indicator of
    "left"  ->  windowGo L False
    "right" ->  windowGo R False
    _       ->  viewDesktop' indicator
    
viewDesktop' numStr = view2 x y
    where (x,y) = myDesktops !! num 
          num = f (read numStr::Int)
          f x
              | x == 10 = 0
              | x > 10 = x - 1
              | otherwise = x
-- --------------------------------------------------------------
-- screenToWorkspaceId :: W.Screen (W.Workspace i j k) k2 d -> WorkspaceId
screenToWorkspaceId (W.Screen (W.Workspace wsId _ _) _sid _) = wsId

-- | Switch what's viewed on 1st and 2nd screens:
-- Get a focusted and an unfocused screen
-- The shift focus to the unfocused on
swapCurrentViews :: X ()
swapCurrentViews =
  withWindowSet
    (\ss ->
        let W.Screen (W.Workspace wc_id _ _) sc_id _ = W.current ss
            W.Screen (W.Workspace wx_id _ _) sid_x _ = case (W.visible ss) 
                                                         of (x:xs) -> x
                                                            [ ]    -> W.current ss  
        in case (W.visible ss,sc_id) of
             ([], _  ) -> return ()
             (_ , S 0) -> view2 wx_id wc_id
             (_ ,_   ) -> view2 wc_id wx_id)

-- | Call extenal script to swap dsplay mode
xdpyToggle :: X ()
xdpyToggle = 
  do  spawn "xdpytoggle"
      return ()

getXMHome :: IO String
getXMHome =  
  do dirs <- getDirectories
     return $ cfgDir dirs

-- | Help -- creates a menu of desktop names using dmenu, showing names a top of screen
--           User can then select (and will be swithed to) a desktop.
helpWsCommand :: X ()
helpWsCommand
 = do
  xmhome <- liftIO getXMHome
      -- If we don't delay until fifo is created
      -- the shell will create a regular file named "fifo"
  let fifodir = xmhome ++ "/run"
  let fifo = fifodir ++ "/fifo"
  make_fifo fifodir fifo
      -- menu_to_fifo: "xmesage>>fifo" blocks unless it runs
      --  after fifo is read from.
  menu_to_fifo fifo
  result <- io (get_result fifo)
  trace $ "xmonad: Switching view to `" ++ result ++ "'"
  windows $ W.greedyView result
  where
    make_fifo fifodir fifo = do
      already <- io $ doesFileExist fifo
      if already
        then return ()
        else do
          _mkfifo fifodir fifo
          io $ threadDelay 1000000
    _mkfifo fifodir fifo =
      spawn $
      "fifodir=" ++
      fifodir ++
      "; fifo=" ++
      fifo ++
      ";\
               \[  -e $fifo ] || { \
               \[ -e $fifodir ] || mkdir -p $fifodir;\
               \mkfifo $fifo ;}"
    menu_to_fifo :: String -> X ()
    menu_to_fifo fifo = do
      let dmbuttons = unlines myWorkspaces
      spawn $ "echo '" ++ dmbuttons ++ "'| dmenu > " ++ fifo
      -- Polls fifo for a result
    get_result fifo = withFile fifo ReadMode getIt

-- | Read result from menu of workspaces
--  waitval is (-1) because a real value crashes 
getIt h = do
  catch tryit tryit_again
  where
    tryit = do
      trace "getIt..."
      ready <- hWaitForInput h (-1)
      if ready
        then put_result
        else trace "xmonad: WTF" >> return ""
    tryit_again e = do
      hPutStrLn stderr $ "E :-" ++ show (e :: IOError)
      trace "xmonad: Waiting for input..."
      threadDelay delayusecs
      getIt h
    put_result = do
      result <- hGetLine h
      trace $ "xmonad: RESULT: `" ++ result ++ "'."
      return result
    delayusecs = round $ delaysecs * 1e6
    delaysecs = 0.5


helpCommand :: X ()
helpCommand = do xmhome <- liftIO getXMHome
                 let helpfile = xmhome ++ "/doc/keys.org"
                 spawn $ "xmessage -file " ++ helpfile
--
-- vim: set expandtab tabstop=4 shiftwidth=4 ai:
