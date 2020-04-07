{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Actions.Maximize
    ( module Actions.Maximize
    ) where

import           XMonad
import qualified XMonad.StackSet    as W

import           Actions.Navigation
import           Layout.Maximize

import           Data.List
import           Data.Maybe


maximize, forcemaximize, unmaximize :: X ()
maximize = withFocused $ sendMessage . ToggleMaximize
forcemaximize = withFocused $ sendMessage . Maximize
unmaximize = sendMessage UnMaximize

swapMasterOrMaximize :: X ()
swapMasterOrMaximize =
    do w <- focusedWindow
       alreadyMaster <- fromMaybe (return False) $ isMaster <$> w
       if alreadyMaster
       then maximize
       else do unmaximize; swapMaster
    where focusedWindow = gets $ W.peek . windowset
          isMaster w = gets $ (== Just 0) . findIndex (==w) . W.index . windowset
