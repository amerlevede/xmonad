
-- | Module that allows binding double click gestures to actions.
--   Use as: (gesture) ==> (action1) ++> (action2)
--   Works with either mouse or key gestures.
--   TODO: Implement support for triple- and DoubleClick events.
--   TODO: Implement variant where first action is only executed when no double click follows.
module Basic.DoubleClick
    ( module Basic.DoubleClick
    ) where

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as Ex

import           Basic.KeyBindings
import qualified Memory.Input

import           Data.Map                    (Map)
import qualified Data.Map.Strict             as M
import           Data.Time.Clock
import           Data.Time.Clock.POSIX



data DoubleClick x y = DoubleClick { delay :: POSIXTime, onsingle :: x, ondouble :: y }

(++>) :: (Action x, Action y) => x -> y -> DoubleClick x y
x ++> y = DoubleClick { delay = 0.2, onsingle = x, ondouble = y }

instance (Action x, Action y) => Action (DoubleClick x y) where

    toKeyAction conf gesture@(gesturemask, gesturekey) DoubleClick { delay = d, onsingle = single, ondouble = double } =
        do now <- io getPOSIXTime
           lastev <- Memory.Input.last
           case lastev of
              Just (Memory.Input.Key lasttime lastmask lastkey)
                      |    lastmask == gesturemask
                        && lastkey  == gesturekey
                        && lasttime >= now - d
                  ->  toKeyAction conf gesture double
              _   ->  toKeyAction conf gesture single

    toMouseAction conf gesture@(gesturemask, gesturebutton) DoubleClick { delay = d, onsingle = single, ondouble = double } w =
        do now <- io getPOSIXTime
           lastev <- Memory.Input.last
           trace (show lastev)
           case lastev of
              Just (Memory.Input.Mouse lasttime lastmask lastbutton)
                      |    lastmask   == gesturemask
                        && lastbutton == gesturebutton
                        && lasttime   >= now - d
                  ->  toMouseAction conf gesture double w
              _   ->  toMouseAction conf gesture single w
