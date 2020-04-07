{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Module for controlling which windows should be floated when spawned.

module Basic.Float
    ( module Basic.Float
    ) where

import           XMonad

import           Basic.ConfigModifier

floatWhen :: [Query Bool] -> ConfigModifier
floatWhen x conf = conf {
    manageHook = manageHook conf <+> foldMap (--> doFloat) x
    }
