
module Actions.Navigation
    ( module Actions.Navigation
    ) where

import           XMonad
import qualified XMonad.StackSet as W

import           Basic

focusUp, focusDown :: X ()
focusUp = windows W.focusUp
focusDown = windows W.focusDown

focus :: Window -> X ()
focus = XMonad.focus

dragFloatMove, dragFloatResize :: Window -> X ()
dragFloatMove = mouseMoveWindow
dragFloatResize = mouseResizeWindow

sink :: Window -> X ()
sink = windows . W.sink

swapUp, swapDown, swapMaster, shiftMaster :: X ()
swapUp = windows W.swapDown
swapDown = windows W.swapUp
shiftMaster = windows W.shiftMaster
swapMaster = windows W.swapMaster

closeWindow :: X ()
closeWindow = kill

masterExpand, masterShrink :: X ()
masterExpand = sendMessage Expand
masterShrink = sendMessage Shrink

masterIncrement, masterDecrement :: X ()
masterIncrement = sendMessage (IncMasterN 1)
masterDecrement = sendMessage (IncMasterN (-1))

cycleLayout :: X ()
cycleLayout = sendMessage NextLayout
resetLayout :: XConfig Layout -> X ()
resetLayout conf = setLayout (layoutHook conf)

-- -- -- | Replace an action that depends on a workspace id, with an action that depends on the index of the id in the configuration's workspace list
-- onWorkspaceIndex :: Action a => (WorkspaceId -> a) -> Int -> XConfig Layout -> a
-- onWorkspaceIndex f i conf =
--     mapM_ (($ conf) . f) $ workspaces conf `safeIndex` i
--     where safeIndex lst j | j < 0 = Nothing
--                           | j >= length lst = Nothing
--                           | otherwise = Just (lst !! j)

focusOnWorkspace :: WorkspaceId -> X ()
focusOnWorkspace = windows . W.greedyView

focusedWindowToWorkspace :: WorkspaceId -> X ()
focusedWindowToWorkspace = windows . W.shift
windowToWorkspace :: WorkspaceId -> Window -> X ()
windowToWorkspace = ((.).(.)) windows W.shiftWin
