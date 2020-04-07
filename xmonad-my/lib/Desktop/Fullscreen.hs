{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Desktop.Fullscreen
    ( module Desktop.Fullscreen
    ) where

import           XMonad

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutModifier

import           Basic

import           Data.List

fullscreenFull :: ConfigModifier' l (ModifiedLayout FullscreenFull l)
fullscreenFull = fullscreenSupport

fullscreenWindow :: ConfigModifier' l (ModifiedLayout FullscreenWindow l)
fullscreenWindow conf = conf {
      layoutHook = ModifiedLayout (FullscreenWindow []) (layoutHook conf)
    , handleEventHook = handleEventHook conf <+> fullscreenEventHook
    , manageHook = manageHook conf <+> fullscreenManageHook
}

data FullscreenWindow a = FullscreenWindow [a]
     deriving (Read, Show)

instance LayoutModifier FullscreenWindow Window where
  pureMess ff@(FullscreenWindow fulls) m = case fromMessage m of
    Just (AddFullscreen win) -> Just $ FullscreenWindow $ nub $ win:fulls
    Just (RemoveFullscreen win) -> Just $ FullscreenWindow $ delete win $ fulls
    Just FullscreenChanged -> Just ff
    _ -> Nothing

  pureModifier (FullscreenWindow fulls) rect _ list =
    (visfulls ++ rest, Nothing)
    where (visfulls, rest) = partition (flip elem fulls . fst) list
