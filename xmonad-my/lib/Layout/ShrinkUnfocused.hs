
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Layout.ShrinkUnfocused
    ( module Layout.ShrinkUnfocused
    ) where

import qualified XMonad.StackSet as W

import           Basic

data ShrinkUnfocused l a = ShrinkUnfocused Int (l a)
    deriving (Show, Read)

instance (Eq a, LayoutClass l a) => LayoutClass (ShrinkUnfocused l) a where
    runLayout w rect =
        do (rects, result) <- runLayout w { W.layout = underlayout } rect
           return (map shrinkIfUnfocused rects, fmap (ShrinkUnfocused shrinkage) result)
        where ShrinkUnfocused shrinkage' underlayout = W.layout w
              shrinkage = fromIntegral shrinkage'
              focused = fmap W.focus . W.stack $ w
              shrink Rectangle { rect_x=x, rect_y=y, rect_width=w, rect_height=h } =
                  Rectangle { rect_x=x+shrinkage, rect_y=y+shrinkage, rect_width=w-2*shrinkage, rect_height=h-2*shrinkage }
              shrinkIfUnfocused (w,r) = if Just w == focused then (w,r) else (w,shrink r)

shrinkUnfocused :: Int -> ConfigModifier' l (ShrinkUnfocused l)
shrinkUnfocused n conf = conf {
    layoutHook = ShrinkUnfocused n $ layoutHook conf
    }
