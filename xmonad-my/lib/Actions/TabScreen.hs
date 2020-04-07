{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Actions.TabScreen
    ( module Actions.TabScreen
    ) where

import           Control.Monad

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as Ex

import           TabScreen

tabclose :: X ()
tabclose =
    do curwsid <- gets $ W.currentTag . windowset
       tabdata <- Ex.get
       when (Just curwsid == tab_wsid tabdata) $
           case tab_behind tabdata of
              Just wsid -> do windows $ W.greedyView wsid
                              Ex.put $ tabdata { tab_behind = Nothing }
              Nothing -> return ()

tabopen :: X ()
tabopen =
    do curwsid <- gets $ W.currentTag . windowset
       tabdata <- Ex.get
       unless (Just curwsid == tab_wsid tabdata) $
           case tab_behind tabdata of
               Just wsid -> do Ex.put $ tabdata { tab_behind = Nothing } -- Should make this properly shut down tab in other screeen for proper xinemera support
                               tabopen
               Nothing -> case tab_wsid tabdata of
                   Just wsid -> do windows $ W.greedyView wsid
                                   Ex.put $ tabdata { tab_behind = Just curwsid }
                   Nothing -> return ()

tabtoggle ::  X ()
tabtoggle =
    do tabdata <- Ex.get
       curwsid <- gets $ W.currentTag . windowset
       if Just curwsid == tab_wsid tabdata
       then tabclose
       else tabopen

withTabscreen :: (WorkspaceId -> X ()) -> X ()
withTabscreen f =
    do tabdata <- Ex.get
       case tab_wsid tabdata of
           Just tabwsid -> f tabwsid
           Nothing      -> return ()
