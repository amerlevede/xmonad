
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Layout modifier that helps keep track of recently visited workspaces

module History
    ( module History
    ) where

import           XMonad
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as Ex

import           ConfigModifier

import           Control.Monad
import qualified Data.Map.Strict             as M

data History = History (M.Map ScreenId [WorkspaceId])
instance ExtensionClass History where
    initialValue = History M.empty

data WithHistory l w = WithHistory (l w)
    deriving (Read, Show)

instance (LayoutClass l w, Show w) => LayoutClass (WithHistory l) w where
    runLayout ws@W.Workspace{ W.layout = WithHistory l } rect =
        do allscreens <- gets (W.screens . windowset)
           forM_ allscreens $ \ screen ->
               do History history <- Ex.get
                  let wtag = W.tag . W.workspace $ screen
                  let stag = W.screen $ screen
                  case history M.!? stag of
                      Nothing -> Ex.modify (\(History m) -> History (M.insert stag [wtag] m))
                      Just [] -> Ex.modify (\(History m) -> History (M.insert stag [wtag] m))
                      Just (wtag':rest) | wtag == wtag'
                              -> Ex.modify (\(History m) -> History (M.adjust (take 20 . (wtag :)) stag m))
                      _       -> return ()
           (result, ul) <- runLayout ws { W.layout = l } rect
           return (result, WithHistory <$> ul)

    handleMessage (WithHistory l) msg =
        do undermsg <- handleMessage l msg
           return $ WithHistory <$> undermsg
