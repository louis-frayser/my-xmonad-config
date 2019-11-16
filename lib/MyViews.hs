module MyViews (myWorkspaces,myExtraWorkspaces,swapCurrentViews,
  view2)
{- | View & View-controller definitions that must be shared between xmonad and config modules
 -}
where
--
-- Part of xmonad config file.
--
import XMonad
import XMonad.Actions.OnScreen
-- 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
--
import Graphics.X11.ExtraTypes.XF86   -- KBD Key names
import Data.Maybe
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

-- | Switch what's viewed on 1st and 2nd screens
-- Get focusted and and unfocused screen
-- Focus the unfocused on
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
              

     
