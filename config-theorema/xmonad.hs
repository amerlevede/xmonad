
import qualified XMonad.StackSet                  as W

import qualified Actions                          as Actions
import qualified Actions.Desktop.Plasma           as Actions
import           Actions.Rotation                 (Direction (..))
import           Basic
import           Desktop.Fullscreen
import           Desktop.Plasma
import           Layout.EmptyWindows
import           Layout.FocusIsMaster
import           Layout.Maximize
import           Layout.OptionalBorder
import qualified Memory.Input
import           TabScreen

import           XMonad.Hooks.EwmhDesktops        (ewmh)
import qualified XMonad.Hooks.ManageDocks         as Docks
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing

import           Control.Monad
import           Data.Char
import           Data.List

main :: IO ()
main = Actions.launchXmonad $ def
    & setLayout
    & setModMask myModMask
    & setKeys myKeys
    & setMouseBindings myMouse
    & setTerminal myTerminal
    & setWorkspaces myWorkspaces

    & activateCapsAsSuper
    -- & atStartup (mapM_ (sendMessageWithNoRefresh Docks.ToggleStruts) . filter ((==myTabWorkspace) . W.tag) . W.workspaces . windowset =<< get)

    & floatWhen myFloats
    & borderForFloats

    -- & xwinBackground (ImageFromFile "/home/adriaan/Pictures/normalized.jpg")
    & activateTransparency
    & activateTabscreen' myTabWorkspace myTabscreenLayout
    & activateMaximize

    & ignoreDocks
--    & avoidStruts
    & fixPlasma
    & fullscreenWindow

    & Memory.Input.log


myModMask = mod4Mask
myTerminal = "konsole"
myBrowser = "firefox -new-window"
myWorkspaces = map show [1..5]
myTabWorkspace = "tab"

myFloats =
    -- Mathematica (float find/replace and splash windows)
    [ ("Find and Replace" ==) <$> stringProperty "WM_NAME"
    , ("Welcome to Wolfram Mathematica" ==) <$> stringProperty "WM_NAME"
    -- Gimp (float all)
    , (isPrefixOf "gimp" . map toLower) <$> className
    -- Chrome profile error
    , (("profile error occurred" ==) . map toLower) <$> stringProperty "WM_NAME"
    -- matplotlib plots
    , ("matplotlib" ==) <$> className
    ]

setLayout conf =
    conf { layoutHook = myLayout
         , borderWidth = 5
         , normalBorderColor = myNonfocusColor
         , focusedBorderColor = myFocusColor }
    where myLayout = tall ||| wide
          tall =          Tall 1 (5/100) (1/2)
          wide = Mirror $ Tall 1 (5/100) (3/4)
          addTitleBars = noFrillsDeco shrinkText myTitlebarTheme
          myTitlebarTheme = Theme
              { activeColor = myFocusColor
              , inactiveColor = myNonfocusColor
              , urgentColor = myUrgentColor
              , urgentBorderColor = myUrgentColor
              , activeBorderColor = myFocusColor
              , inactiveBorderColor = myNonfocusColor
              , activeTextColor = myFocusColor
              , inactiveTextColor = myNonfocusColor
              , urgentTextColor = myFocusColor
              , fontName = "Roboto"
              , decoWidth = decoWidth def
              , decoHeight = 8
              , windowTitleAddons = windowTitleAddons def
              , windowTitleIcons = windowTitleIcons def
              }
          myNonfocusColor = "#99aa0"--"#dddee0"
          myFocusColor = "#000000"
          myUrgentColor = "#dd000000"

myTabscreenLayout = spacingWithEdge 17 $ FocusIsMaster $ EmptyWindows 2 $ Tall 1 (5/100) (3/4)

myKeys :: [KeyBinding]
myKeys =
    [ ModShift xK_a         ==> sendMessage Docks.ToggleStruts ]
    ++
    -- window navigation keys
    [ Mod      xK_Tab       ==> Actions.focusDown
    , Mod      xK_backslash ==> Actions.focusUp
    , ModShift xK_Tab       ==> Actions.swapUp
    , ModShift xK_backslash ==> Actions.swapDown
    , Mod      xK_q         ==> Actions.closeWindow
    ]
    ++
    -- layout navigation keys
    [ Mod      xK_equal     ==> Actions.masterExpand
    , Mod      xK_minus     ==> Actions.masterShrink
    , ModShift xK_equal     ==> Actions.masterIncrement
    , ModShift xK_minus     ==> Actions.masterDecrement

    , Mod      xK_space     ==> Actions.cycleLayout
    , ModShift xK_space     ==> Actions.resetLayout
    ]
    ++
    -- workspace navigation keys
    concat
        [ [ Mod      key    ==> Actions.focusOnWorkspace wsid
          , ModShift key    ==> Actions.focusedWindowToWorkspace wsid ]
        | (key, wsid) <- zip [xK_1 .. xK_9] myWorkspaces ]
    ++
    -- rotation keys
    [ ModShift xK_Up        ==> Actions.rotate North
    , ModShift xK_Right     ==> Actions.rotate East
    , ModShift xK_Down      ==> Actions.rotate South
    , ModShift xK_Left      ==> Actions.rotate West
    ]
    ++
    -- fullscreen keys
    [ Mod      xK_f         ==> Actions.maximize
    , Mod      xK_Return    ==> Actions.swapMasterOrMaximize
    , ModShift xK_Return    ==> Actions.maximize
    ]
    ++
    -- tabscreen keys
    [ Mod      xK_grave     ==> Actions.withTabscreen Actions.focusOnWorkspace
    , ModShift xK_grave     ==> Actions.withTabscreen Actions.focusedWindowToWorkspace
    , Mod      xK_o         ==> Actions.tabtoggle
    , ModShift xK_o         ==> Actions.withTabscreen Actions.focusedWindowToWorkspace
    ]
    ++
    -- session control keys
    [ Mod      xK_r         ==> Actions.restartXmonad
    , ModShift xK_r         ==> Actions.restartXmonadFull
    , Mod      xK_Escape    ==> Actions.endSession
    , ModShift xK_Escape    ==> Actions.endSessionHard
    ]
    ++
    -- other
    [ Mod xK_t              ==> Actions.spawnTerminal
    , Mod xK_b              ==> Actions.spawn myBrowser
    , Mod xK_BackSpace      ==> Actions.spawn "krunner"
    ]

myMouse :: [MouseBinding]
myMouse =
    [ Mod button1 ==> Actions.focus *** Actions.dragFloatMove
                  ++> ifTargetIsFocus Actions.sink
    , Mod button3 ==> Actions.focus *** Actions.dragFloatResize
    , ModShift button1 ==> Actions.focus *** Actions.dragFloatResize
    ]
