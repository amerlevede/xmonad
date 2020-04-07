
module Actions.Basic
    ( module Actions.Basic
    ) where

import           XMonad

spawn :: String -> X ()
spawn = XMonad.spawn

spawnTerminal :: XConfig Layout -> X ()
spawnTerminal = XMonad.spawn . terminal
