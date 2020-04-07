
-- | Compiles all actions from the Actions folder.
-- | Best to import qualified.

module Actions
    ( module Actions.Rotation
    , module Actions.Navigation
    , module Actions.Brightness
    , module Actions.TabScreen
    , module Actions.Volume
    , module Actions.XMonad
    , module Actions.Maximize
    , module Actions.Basic
    ) where

import           Actions.Basic
import           Actions.Brightness
import           Actions.Maximize
import           Actions.Navigation
import           Actions.Rotation
import           Actions.TabScreen
import           Actions.Volume
import           Actions.XMonad
