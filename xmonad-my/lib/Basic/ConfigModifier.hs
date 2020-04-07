{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- | Module contains shorthand types for functions that modify the config type.

module Basic.ConfigModifier
    ( module Basic.ConfigModifier
    ) where

import           XMonad

(&) = flip ($)

type ConfigModifier' l l' = LayoutClass l Window => XConfig l -> XConfig l'
type ConfigModifier = forall l. ConfigModifier' l l

modifyLayout :: (l Window -> l' Window) -> ConfigModifier' l l'
modifyLayout f conf = conf { layoutHook = f (layoutHook conf) }
