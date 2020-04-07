
-- | Actions pertaining to screen brightness (on a laptop)
--   NOTE: These actions require xbacklight

module Actions.Brightness
    ( module Actions.Brightness
    ) where

import           XMonad
import qualified XMonad.StackSet as W

brightnessUp, brightnessDown :: Int -> X ()
brightnessUp val = spawn $ "xbacklight -inc " ++ show val
brightnessDown val = spawn $ "xbacklight -dec " ++ show val

-- | Previously used settings
-- myBrightnessControlKeys :: [KeyBinding]
-- myBrightnessControlKeys =
--     [ Mod xK_End      ==> brightnessUp 10
--     , Mod xK_F12      ==> brightnessUp 10
--     , ModShift xK_End ==> brightnessUp 1
--     , ModShift xK_F12 ==> brightnessUp 1
--     , Mod xK_Home     ==> brightnessDown 10
--     , Mod xK_F11      ==> brightnessDown 10
--     , ModShift xK_Home ==> brightnessDown 1
--     , ModShift xK_F11  ==> brightnessDown 1
--     ] -- TODO: Add brightness button
--
