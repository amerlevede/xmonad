{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Layout modifier that theplays the focused element in the master area.
--   This is different from XMonad.Layout.MagicFocus in that it leaves empty space where the focused element would otherwise be.

module Layout.FocusIsMaster
    ( module Layout.FocusIsMaster
    ) where

import           XMonad
import qualified XMonad.StackSet as W

newtype FocusIsMaster l a = FocusIsMaster (l a)
    deriving (Read, Show)

instance LayoutClass l a => LayoutClass (FocusIsMaster l) a where
    runLayout ws rect =
        case W.stack ws of
            Nothing -> runUnderlyingLayout
            Just s -> let pseudostack = s { W.up = [], W.down = reverse (W.up s) ++ [W.focus s] ++ W.down s }
                          n = length (W.up s)
                      in do (rects, result) <- runLayout ws { W.layout = ul, W.stack = Just pseudostack } rect
                            return $ (take (n+1) rects ++ drop (n+2) rects, FocusIsMaster <$> result)
        where FocusIsMaster ul = W.layout ws
              runUnderlyingLayout =
                  do (rects, result) <- runLayout ws { W.layout = ul } rect
                     return (rects, FocusIsMaster <$> result)

    handleMessage (FocusIsMaster ul) msg =
        do result <- handleMessage ul msg
           return $ FocusIsMaster <$> result
