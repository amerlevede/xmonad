{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memory.Handle
    ( module Memory.Handle
    ) where

import           XMonad
import qualified XMonad.Util.ExtensibleState as Ex

import           System.Exit                 (ExitCode (..))
import           System.Posix.Types          (ProcessID)
import           System.Process              (readProcessWithExitCode)

data Handle tag = Handle tag

data HandlePID tag = HandlePID (Maybe ProcessID)
    deriving (Show, Read, Typeable)

class (Show tag, Read tag, Typeable tag) => HandleTag tag where
    summonHandle :: proxy tag -> String

instance HandleTag tag => ExtensionClass (HandlePID tag) where
    initialValue = HandlePID Nothing
    extensionType = PersistentExtension

getHandleMaybe :: forall proxy tag. HandleTag tag => proxy tag -> X (Maybe ProcessID)
getHandleMaybe _ = do HandlePID r <- Ex.get :: X (HandlePID tag)
                      return r

getHandle :: forall proxy tag. HandleTag tag => proxy tag -> X ProcessID
getHandle proxytag =
    getHandleMaybe proxytag >>= \ case
        Just pid ->
         do liftIO (readProcessWithExitCode "kill" ["-0", show pid] "") >>= \ case
                (ExitSuccess, _, _) -> return pid
                (ExitFailure _, _, _) -> forceRefreshHandle proxytag
        Nothing -> forceRefreshHandle proxytag

forceRefreshHandle :: forall proxy tag. HandleTag tag => proxy tag -> X ProcessID
forceRefreshHandle proxytag =
 do pid <- spawnPID (summonHandle proxytag)
    Ex.put $ (HandlePID (Just pid) :: HandlePID tag)
    return pid

refreshHandle :: HandleTag tag => proxy tag -> X ()
refreshHandle proxytag = getHandle proxytag >> return ()
