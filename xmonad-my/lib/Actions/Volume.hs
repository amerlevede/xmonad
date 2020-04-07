
-- | Actions pertaining to control of speaker volume.
--   NOTE: These actions require amixer.

module Actions.Volume
    ( module Actions.Volume
    ) where

import           XMonad
import qualified XMonad.StackSet as W

volumeUp, volumeDown :: Int -> X ()
volumeUp val = spawn $ "amixer sset Master " ++ show val ++ "%+"
volumeDown val = spawn $ "amixer sset Master " ++ show val ++ "%-"

volumeToggle :: X ()
volumeToggle = spawn "amixer sset Master toggle && amixer sset Speaker unmute && amixer sset Headphone unmute"
