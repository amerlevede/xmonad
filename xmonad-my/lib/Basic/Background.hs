{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}

-- | Allows setting a background image

module Basic.Background
    ( module Basic.Background
    ) where

import           XMonad
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as W

import           Basic.ConfigModifier

import           Control.Monad
import           Data.List
import           Data.Maybe


data ImageSource = ImageFromFolder String | ImageFromFile String
    deriving (Show,Read)

-- | Activate window transparency by opening an X composition manager.
-- NOTE: requires xcompmgr
activateTransparency :: ConfigModifier
activateTransparency conf =
    conf { startupHook = startupHook conf >> spawn cmd }
    where cmd = "xcompmgr"

-- | Set the root background image.
--   This is how the background is ideally set, but will not work with desktop environments such as KDE which put their own desktop layer on top of the root window.
--   NOTE: Requires feh
rootBackground :: ImageSource -> ConfigModifier
rootBackground (ImageFromFile img) conf =
    conf { startupHook = spawn cmd >> startupHook conf }
    where cmd = "feh --bg-fill " ++ img
rootBackground (ImageFromFolder folder) conf =
    conf { startupHook = spawn cmd >> startupHook conf }
    where cmd = "feh --bg-fill --randomize " ++ folder

-- -- | Set background image by opening a new X window which contains the image, and putting that window in the background layer.
-- --   NOTE: Requires xloadimage
xwinBackground :: ImageSource -> ConfigModifier' l (Background l)
xwinBackground src conf =
    conf { layoutHook = Background src NotYetSpawned (layoutHook conf)
         , manageHook = manageHook conf <+>
                        (className =? "Xloadimage" -->
               do w <- ask
                  liftX $ sendMessage $ SetBackground w
                  return mempty)
         }


data Background l a = Background ImageSource BackgroundState (l a)
    deriving (Show, Read)

xview :: ImageSource -> X ()
xview (ImageFromFile src) = spawn $ "xview \"" ++ src ++ "\""

unxview :: BackgroundState -> X ()
unxview (Spawned w) = killWindow w
unxview _           = return ()

instance (LayoutClass l Window) => LayoutClass (Background l) Window where
    runLayout ws@W.Workspace{ W.layout = Background src NotYetSpawned l } rect =
        do xview src
           runLayout ws{ W.layout = Background src Spawning l } rect
    runLayout ws@W.Workspace{ W.layout = Background src Spawning l, W.tag = t } rect =
        do broadcastMessage HideBackground
           (rs, ml') <- runLayout (ws {W.layout=l}) rect
           return (rs, Background src Spawning <$> ml')
    runLayout ws@W.Workspace{ W.layout = Background src (Spawned b) l, W.tag = t } rect =
        do modify $ \st -> st { windowset = modifyWorkspace t (windowset st) (removeFromStack b) }
           (rs, ml') <- runLayout (removeFromStack b ws){W.layout=l} rect
           return (rs ++ [(b, rect')], Background src (Spawned b) <$> ml')
        where rect' = rect { rect_width = rect_width rect `div` 2, rect_height = rect_height rect `div` 2 }

    handleMessage (Background src mb l) msg
        | Just (SetBackground b) <- fromMessage msg
          = return . Just $ Background src (Spawned b) l
        | Just NoBackground <- fromMessage msg
          = return . Just $ Background src NotYetSpawned l
        | Just HideBackground <- fromMessage msg
          = case mb of Spawned w -> do killWindow w; return . Just $ Background src NotYetSpawned l
                       _         -> return Nothing
        | otherwise
          = fmap (Background src mb) <$> handleMessage l msg

removeFromStack win w@W.Workspace { W.stack = Nothing } = w
removeFromStack win w@W.Workspace { W.stack = Just sta} = w { W.stack = W.filter (/=win) sta }
addToStack win w@W.Workspace { W.stack = Nothing } = w { W.stack = Just W.Stack { W.focus=win, W.up=[], W.down=[] } }
addToStack win w@W.Workspace { W.stack = Just sta} = w { W.stack = Just sta { W.up = win : W.up sta } }
modifyWorkspace tag wset f = W.mapWorkspace (\w -> if W.tag w == tag then f w else w) wset

data BackgroundState
    = NotYetSpawned
    | Spawning
    | Spawned Window
    deriving (Show,Read)

data BackgroundMessage
    = SetBackground Window -- Set background to this window
    | NoBackground         -- Remove background
    | HideBackground       -- Hide the background. Since backgrounds are not in the StackSet, this is necessary to hide the background when switching workspaces.
    deriving (Read, Show)
instance Message BackgroundMessage
