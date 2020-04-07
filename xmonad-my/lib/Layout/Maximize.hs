{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | Config modifier that allows putting a window at full-screen.
--   Full-screen setting is automatically removed when the window loses focus.

module Layout.Maximize
    ( module Layout.Maximize
    ) where

import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as W

import           Basic

import           Control.Applicative
import           Control.Monad
import qualified Data.Map                     as M
import           Data.Maybe


data MaximizeMessage = Maximize Window | UnMaximize | ToggleMaximize Window
    deriving (Typeable)
instance Message MaximizeMessage

activateMaximize :: ConfigModifier' l (ModifiedLayout Maximizable l)
activateMaximize conf = conf { layoutHook = ModifiedLayout (Maximized Nothing) (layoutHook conf) }

-- | Layout modifier that allows maximizing a window.
--   Control maximization by sending a MaximizeMessage.
--   Goes back to normal layout after appropriate MaximizeMessage, any ChangeLayoutMessage, and when the maximized window loses the focus of its workspace.
newtype Maximizable a = Maximized (Maybe a)
    deriving (Show, Read)
instance LayoutModifier Maximizable Window where
    handleMess (Maximized oldfull) msg =
        (<|>) <$> handleChangeLayoutMessage
              <*> handleMaximizeMessage
        where maximize w = do focus w; windows (W.sink w); return . Just $ Maximized (Just w)
              unmaximize = do return . Just  $ Maximized Nothing
              handleMaximizeMessage = case fromMessage msg of
                  Just (Maximize w)       -> maximize w
                  Just UnMaximize         -> unmaximize
                  Just (ToggleMaximize w) -> case oldfull of
                      Nothing -> maximize w
                      Just _  -> unmaximize
                  Nothing             -> return Nothing
              handleChangeLayoutMessage = case fromMessage msg of
                  Just NextLayout  -> unmaximize
                  Just FirstLayout -> unmaximize
                  Nothing          -> return Nothing

    -- | If no fullscreen target is set, execute underlying layout
    modifyLayoutWithUpdate (Maximized maxstate) workspace rect
        | Nothing <- maxstate = unmaximize
        | Just w  <- maxstate = fullIfFocused w
        where unmaximize =
                  do underlying <- runLayout workspace rect
                     return (underlying, Just (Maximized Nothing))
              full w =
                  do (frames, _) <- runLayout workspace{W.layout = Full} rect
                     return ((frames, Nothing), Nothing)
              fullIfFocused w =
                  do focusedWindow <- gets $ W.peek . windowset
                     focusedFloats <- gets $ (liftA2 M.member focusedWindow) . Just . W.floating . windowset
                     if maybe False (w==) focusedWindow || fromMaybe False focusedFloats
                     then full w
                     else unmaximize
