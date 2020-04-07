{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Config modifier that allows "tabbing" to switch to or from a particular workspace.
--   See also Actions.TabScreen.

module TabScreen
    ( module TabScreen
    ) where

import           XMonad

import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet              as W
import qualified XMonad.Util.ExtensibleState  as Ex

import           Basic
import           Memory.Handle

-- | NOTE: Other layout modifiers that should apply to both the regular and tabscreen layouts should come after this.
activateTabscreen' :: LayoutClass l' Window => WorkspaceId -> l' Window -> ConfigModifier' l (WithTabscreen l' l)
activateTabscreen' tabwsid tablay conf =
    conf { layoutHook = WithTabscreen (onWorkspace tabwsid tablay (layoutHook conf))
         , startupHook = startupHook conf
               >> refreshHandle xcapeHandle
               >> Ex.put TabData { tab_wsid = Just tabwsid, tab_behind = Nothing }
         , workspaces = workspaces conf ++ [tabwsid]
         }

activateTabscreen :: WorkspaceId -> ConfigModifier' l (WithTabscreen l l)
activateTabscreen tabwsid conf =
    activateTabscreen' tabwsid (layoutHook conf) conf

xcapeCommand = "xcape -e \"Super_L=Super_L|O\""
xcapeHandle = Handle XCape
data XCape = XCape deriving (Show, Read)
instance HandleTag XCape where summonHandle _ = xcapeCommand

newtype WithTabscreen l' l w = WithTabscreen (PerWorkspace l' l w) deriving (Read, Show)

data TabData = TabData
    { tab_wsid   :: Maybe WorkspaceId
    , tab_behind :: Maybe WorkspaceId
    } deriving (Show, Read)
instance ExtensionClass TabData where
    initialValue = TabData Nothing Nothing

data TabMessage = TabOpen | TabClose | TabToggle
    deriving (Show, Read, Typeable)
instance Message TabMessage

instance (LayoutClass l' w, LayoutClass l w, Show w) => LayoutClass (WithTabscreen l' l) w where
    runLayout ws@W.Workspace{ W.layout = WithTabscreen l } rect =
        do (result, ul) <- runLayout ws { W.layout = l } rect
           return (result, WithTabscreen <$> ul)

    handleMessage (WithTabscreen l) msg =
        do undermsg <- handleMessage l msg
           return $ WithTabscreen <$> undermsg
