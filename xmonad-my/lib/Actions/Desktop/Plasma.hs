
module Actions.Desktop.Plasma
    ( module Actions.Desktop.Plasma
    ) where

import XMonad

-- | Lock the screen.
--   Does not work when xmonad was initiated with --restart (not tested otherwise).
--   Default keybinding in KDE: ctrl+alt+L
lockSession :: X ()
lockSession =
    spawn "qdbus org.kde.ksmserver /ScreenSaver org.freedesktop.ScreenSaver.Lock"

-- | Logout gracefully.
--   Default keybinding in KDE: ctrl+alt+del
endSession :: X ()
endSession =
    spawn "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout -1 -1 -1"

-- | Logout ungracefully, on all sessions for this user.
--   Use for emergency shutdown.
endSessionHard :: X ()
endSessionHard =
    spawn "loginctl terminate-user $USER"
