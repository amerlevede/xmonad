{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Desktop.Plasma
    ( module Desktop.Plasma
    ) where

import           Basic
--import           Actions.OptionalBorder

import           XMonad.Hooks.EwmhDesktops    hiding (fullscreenEventHook)
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks     hiding (avoidStruts)
import qualified XMonad.Hooks.ManageDocks     as Docks (avoidStruts)
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutModifier


ignoreDocks :: ConfigModifier
ignoreDocks conf = docks conf
    { manageHook = manageHook conf <+> manageDocks }

avoidStruts :: ConfigModifier' l (ModifiedLayout AvoidStruts l)
avoidStruts conf = conf
    { layoutHook = Docks.avoidStruts $ layoutHook conf }

fixPlasma :: ConfigModifier
fixPlasma conf = conf
    { manageHook = manageHook conf
        <+> (isNotification --> doIgnore)
--        <+> (className =? "krunner" --> turnOffBorder)) -- This isn't working because local version of xmonad-contrib does not have required functionality
    , startupHook = setWMName "LG3D" <+> startupHook conf
    }

isNotification :: Query Bool
isNotification = className =? "plasmashell"
