module MyViews (myWorkspaces,myExtraWorkspaces,
  view2)
{- | View & View-controller definitions that must be shared between config files
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
  let mov2 cs ws = windows $ viewOnScreen cs ws
   in do mov2 0 lft
         mov2 1 rht


