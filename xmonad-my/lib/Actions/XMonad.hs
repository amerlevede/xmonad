{-# LANGUAGE FlexibleContexts #-}

-- | Actions involving the current XMonad process.

module Actions.XMonad
    ( exitXmonad, launchXmonad, restartXmonad, restartXmonadFull
    ) where

import           XMonad
import qualified XMonad.StackSet    as W

import           Control.Monad
import           Data.Function
import           System.Environment
import           System.Exit
import           System.Info
import           System.IO


exitXmonad :: X ()
exitXmonad = io exitSuccess

-- | Restart the currently running Xmonad instance.
restartXmonad :: X ()
restartXmonad = restart "xmonad" True

-- | Restart the currently running Xmonad instance, and forget all metadata.
restartXmonadFull :: X ()
restartXmonadFull = restart "xmonad" False

-- | Launch the XMonad executable, independent of the default xmonad config setup.
--   Xmonad is no automatically recompiled.
--   Use --replace to replace a currently running window manager.
launchXmonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
launchXmonad config =
    do args <- getArgs
       when (args == ["--replace"]) sendReplace
       launch config


sendReplace =
    do dpy <- openDisplay ""
       let dflt = defaultScreen dpy
       rootw  <- rootWindow dpy dflt
       replace dpy dflt rootw

replace :: Display -> ScreenNumber -> Window -> IO ()
replace dpy dflt rootw = do
    -- check for other WM
    wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $ do
        -- prepare to receive destroyNotify for old WM
        selectInput dpy currentWmSnOwner structureNotifyMask

        -- create off-screen window
        netWmSnOwner <- allocaSetWindowAttributes $ \attributes -> do
            set_override_redirect attributes True
            set_event_mask attributes propertyChangeMask
            let screen = defaultScreenOfDisplay dpy
                visual = defaultVisualOfScreen screen
                attrmask = cWOverrideRedirect .|. cWEventMask
            createWindow dpy rootw (-100) (-100) 1 1 0 copyFromParent copyFromParent visual attrmask attributes

        -- try to acquire wmSnAtom, this should signal the old WM to terminate
        xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime

        -- SKIPPED: check if we acquired the selection
        -- SKIPPED: send client message indicating that we are now the WM

        -- wait for old WM to go away
        fix $ \again -> do
            evt <- allocaXEvent $ \event -> do
                windowEvent dpy currentWmSnOwner structureNotifyMask event
                get_EventType event

            when (evt /= destroyNotify) again
