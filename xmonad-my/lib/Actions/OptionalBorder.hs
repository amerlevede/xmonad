
-- Cannot compile: need newer version of xmonad-contrib

module Actions.OptionalBorder 
    ( module Actions.OptionalBorder
    ) where

import XMonad

import XMonad.Layout.NoBorders (HasBorder)

import Control.Monad.Reader

turnOffBorder :: ManageHook
turnOffBorder = Query . ReaderT $ \ w ->
    withDisplay $ \d ->
     do io $ setWindowBorderWidth d w 0
        sendMessage (HasBorder False w)
        return mempty

---- | Copied from xmonad-contrib XMonad.Actions.NoBorders
--toggleBorder :: Window -> X ()
--toggleBorder w = do
--    bw <- asks (borderWidth . config)
--    withDisplay $ \d -> io $ do
--        cw <- wa_border_width `fmap` getWindowAttributes d w
--        if cw == 0
--            then setWindowBorderWidth d w bw
--            else setWindowBorderWidth d w 0
--
--toggleBorderQ :: ManageHook
--toggleBorderQ = Query $ ReaderT toggleBorder >> return mempty
--
--setBorder :: Dimension -> Window -> X ()
--setBorder x w = do
--    withDisplay $ \d -> io $ do
--        setWindowBorderWidth d w x
--
--setBorderQ :: Dimension -> ManageHook
--setBorderQ x = Query $ ReaderT (setBorder x) >> return mempty
