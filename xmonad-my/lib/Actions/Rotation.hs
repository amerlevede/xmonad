
module Actions.Rotation
    ( module Actions.Rotation
    ) where

import           XMonad
import qualified XMonad.StackSet as W

data Direction = North | East | South | West

-- NOTE: requires xrandr
rotate :: Direction -> X ()
rotate d =
    do spawn $ xrandrcmd d
       spawn $ xinputcmd d
    where xrandrcmd dir = "xrandr --output `xrandr -q | grep -oP \".*(?= connected)\"` --rotate " ++ xrandrdir dir
          xinputcmd dir = "xinput set-prop 'ELAN Touchscreen' 'Coordinate Transformation Matrix' " ++ xinputtrans dir
          xrandrdir North = "normal"
          xrandrdir East  = "right"
          xrandrdir South = "inverted"
          xrandrdir West  = "left"
          xinputtrans North = "1 0 0 0 1 0 0 0 1"
          xinputtrans East  = "0 -1 1 1 0 0 0 0 1"
          xinputtrans South = "-1 0 1 0 -1 1 0 0 1"
          xinputtrans West  = "0 1 0 -1 0 1 0 0 1"
