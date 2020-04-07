{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Module containing wrappers for basic XConfig setup, i.e. everything that does not fit in other categories.

module Basic.Basic
    ( module Basic.Basic
    ) where

import           XMonad

import           Basic.ConfigModifier

setTerminal :: String -> ConfigModifier
setTerminal term conf =
    conf { terminal = term }

setWorkspaces :: [String] -> ConfigModifier
setWorkspaces spcs conf =
    conf { workspaces = spcs }

atStartup :: X () -> ConfigModifier
atStartup x conf =
    conf { startupHook = startupHook conf >> x }
