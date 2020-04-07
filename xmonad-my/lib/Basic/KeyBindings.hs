{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | Module to deal with XMonad's unwieldy keybinding settings
-- | To use: specify a KeyBinding by using the (==>) operator, e.g.
-- |     Mod xK_b ==> spawn "firefox"
-- | Convert a list of such KeyBinding entries to the XConfig keys field type with "keybindings", or use setKeys as a ConfigModifier.

module Basic.KeyBindings
    ( module Basic.KeyBindings
    ) where

import           XMonad

import           Basic.ConfigModifier

import           Control.Monad
import qualified Data.Map             as M

type KeyGesture    = (KeyMask, KeySym)
type MouseGesture  = (KeyMask, Button)

type KeyAction     =           X ()
type MouseAction   = Window -> X ()

type KeyBinding    = XConfig Layout -> (KeyGesture, KeyAction)
type MouseBinding  = XConfig Layout -> (MouseGesture, MouseAction)

type KeyBindings   = XConfig Layout -> M.Map KeyGesture KeyAction
type MouseBindings = XConfig Layout -> M.Map MouseGesture MouseAction

newtype Mod a = Mod a
newtype ModShift a = ModShift a
newtype NoMod a = NoMod a
newtype Shift a = Shift a
newtype ModShiftAlt a = ModShiftAlt a

class Action a where
    toKeyAction     :: XConfig Layout -> KeyGesture   -> a -> KeyAction
    toMouseAction   :: XConfig Layout -> MouseGesture -> a -> MouseAction
    (***)           :: a -> a -> a
    ifTargetIsFocus :: a -> a
instance (m ~ X) => Action (m ()) where
    toKeyAction   _ _ a   = a
    toMouseAction _ _ a _ = a
    a *** a' = do a; a'
    ifTargetIsFocus a = a
instance (m ~ X) => Action (Window -> m ()) where
    toKeyAction   _ _ a   = withFocused a
    toMouseAction _ _ a w = a w
    a *** a' = \ w -> do a w; a' w
    ifTargetIsFocus a = \ w -> withFocused $ \ w' -> when (w == w') (a w)
instance (m ~ X) => Action (XConfig Layout -> m ()) where
    toKeyAction   c _ a   = a c
    toMouseAction c _ a _ = a c
    a *** a' = \ c -> do a c; a' c
    ifTargetIsFocus a = a
instance (m ~ X) => Action (XConfig Layout -> Window -> m ()) where
    toKeyAction   c _ a   = withFocused (a c)
    toMouseAction c _ a w = a c w
    a *** a' = \ c w -> do a c w; a' c w
    ifTargetIsFocus a = \ c w -> withFocused $ \ w' -> when (w == w') (a c w)

class Gesture f where
    toGesture :: XConfig Layout -> f a -> (KeyMask, a)
instance Gesture Mod where
    toGesture conf (Mod sym) = (modMask conf, sym)
instance Gesture ModShift where
    toGesture conf (ModShift sym) = (modMask conf .|. shiftMask, sym)
instance Gesture NoMod where
    toGesture _ (NoMod sym) = (0, sym)
instance Gesture Shift where
    toGesture _ (Shift sym) = (shiftMask, sym)
instance Gesture ModShiftAlt where
    toGesture conf (ModShiftAlt sym) = (modMask conf .|. shiftMask .|. mod1Mask, sym)

class Binding a x where
    (==>) :: (Gesture f, Action act) => f x -> act -> a
infix 8 ==>
instance Binding KeyBinding KeySym where
    x ==> y = \ c -> let g = toGesture c x in (g, toKeyAction c g y)
instance Binding MouseBinding Button where
    x ==> y = \ c -> let g = toGesture c x in (g, toMouseAction c g y)

keybindings :: [KeyBinding] -> KeyBindings
keybindings lst conf =
    M.fromListWith err $ map ($ conf) lst
    where err _ _ = error $ "Error: The same key combination was mapped to multiple actions."

setKeys :: [KeyBinding] -> ConfigModifier
setKeys ks conf =
    conf { keys = keybindings ks }

mousebindings :: [MouseBinding] -> MouseBindings
mousebindings lst conf =
    M.fromListWith err $ map ($ conf) lst
    where err _ _ = error $ "Error: The same button combination was mapped to multiple actions."

setMouseBindings :: [MouseBinding] -> ConfigModifier
setMouseBindings ms conf =
    conf { mouseBindings = mousebindings ms }

setModMask :: KeyMask -> ConfigModifier
setModMask mask conf =
    conf { modMask = mask }

activateCapsAsSuper :: ConfigModifier
activateCapsAsSuper conf =
    conf { startupHook = spawn cmd >> startupHook conf }
    where cmd = "setxkbmap -option \"caps:super\""
