{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layout.OptionalBorder
    ( module Layout.OptionalBorder
    ) where

import           XMonad
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as W

import           XMonad.Layout.NoBorders

import           Basic

import qualified Data.Map                     as M

borderForFloats :: ConfigModifier' l (ModifiedLayout (ConfigurableBorder FloatOnlyBorders) l)
borderForFloats conf =
    conf { layoutHook =  lessBorders FloatOnlyBorders (layoutHook conf) }

data FloatOnlyBorders = FloatOnlyBorders deriving (Read,Show)


instance SetsAmbiguous FloatOnlyBorders where
    hiddens _ winset _ = filter f . map fst where
        f = not . flip elem floatingwindows
        floatingwindows = M.keys . W.floating $ winset
