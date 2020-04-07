{-# LANGUAGE FlexibleContexts #-}

module Memory.Input
    ( module Memory.Input
    ) where

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as Ex

import           Basic.ConfigModifier

import           Data.Bits
import           Data.List
import           Data.Map                    (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (listToMaybe)
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

data Input = Mouse POSIXTime KeyMask Button
           | Key   POSIXTime KeyMask KeySym
           deriving (Show)
time :: Input -> POSIXTime
time (Mouse x _ _) = x
time (Key   x _ _) = x
mask :: Input -> KeyMask
mask (Mouse _ x _) = x
mask (Key   _ x _) = x
button :: Input -> Maybe Button
button (Mouse _ _ x) = Just x
button _             = Nothing
key :: Input -> Maybe KeySym
key (Key _ _ x) = Just x
key _           = Nothing

newtype InputHistory = InputHistory [Input] deriving (Show)
instance ExtensionClass InputHistory where
    initialValue = InputHistory []

rememberAll :: X [Input]
rememberAll = Ex.gets f where f (InputHistory l) = l

rememberMouse :: X [Input]
rememberMouse = filter f <$> rememberAll
    where f (Mouse _ _ _) = True
          f (Key _ _ _)   = False
rememberKeys :: X [Input]
rememberKeys = filter f <$> rememberAll
    where f (Mouse _ _ _) = False
          f (Key _ _ _)   = True

-- | Get the last recorded Input event (before any that are currently being processed).
last :: X (Maybe Input)
last = do ev <- asks currentEvent
          hist <- Ex.gets $ \(InputHistory l) -> l
          return $ case ev of
              Just KeyEvent {}   -> listToMaybe (drop 1 hist)
              Just ButtonEvent{} -> listToMaybe (drop 1 hist)
              _                  -> listToMaybe hist

-- | Get the Input event that is currently being processed.
this :: X (Maybe Input)
this = do ev <- asks currentEvent
          hist <- Ex.gets $ \(InputHistory l) -> l
          return $ case ev of
              Just KeyEvent {}    -> listToMaybe hist
              Just ButtonEvent {} -> listToMaybe hist
              _                   -> Nothing


lookupMouse :: KeyMask -> Button -> X (Maybe POSIXTime)
lookupMouse m b = Ex.gets $ \ (InputHistory l) -> time <$> find f l
    where f i = mask i == m && button i == Just b
lookupKeys :: KeyMask -> KeySym -> X (Maybe POSIXTime)
lookupKeys m k = Ex.gets $ \ (InputHistory l) -> time <$> find f l
    where f i = mask i == m && key i == Just k

log :: ConfigModifier
log conf = conf { handleEventHook = hook <+> handleEventHook conf}
    where hook KeyEvent { ev_state = uncleanmask, ev_keycode = key } =
              do now <- io getPOSIXTime
                 disp <- asks display
                 sym <- io $ keycodeToKeysym disp key 0
                 mask <- (.&. (255)) <$> cleanMask uncleanmask
                 add $ Key now mask sym
                 mempty
          hook ButtonEvent { ev_state = uncleanmask, ev_button = but } =
              do mask <- (.&. (255)) <$> cleanMask uncleanmask
                 now <- io getPOSIXTime
                 add $ Mouse now mask but
                 mempty
          hook _ = mempty
          add ev = Ex.modify $ \ (InputHistory l) -> InputHistory (take 20 $ ev : l)
