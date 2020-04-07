{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Layout modifier that tells the underlying Layout that there are more windows than there are, resulting in some empty space.

module Layout.EmptyWindows
    ( module Layout.EmptyWindows
    ) where

import           XMonad
import qualified XMonad.StackSet as W

data EmptyWindows l a = EmptyWindows Int (l a)
    deriving (Read, Show)

instance LayoutClass l a => LayoutClass (EmptyWindows l) a where
    runLayout ws rect =
        case W.stack ws of
            Nothing -> runUnderlyingLayout
            Just s -> case 1 + length (W.up s) + length (W.down s) of
                nwins | nwins >= n -> runUnderlyingLayout
                nwins -> do let addn = n - length (W.up s) - 1
                            let phantomstack = s { W.down = take addn . cycle $ W.down s ++ [W.focus s] ++ W.up s }
                            (rects, result) <- runLayout ws { W.layout = ul, W.stack = Just phantomstack } rect
                            return (take nwins rects, EmptyWindows n <$> result)
        where EmptyWindows n ul = W.layout ws
              runUnderlyingLayout =
                  do (rects, result) <- runLayout ws { W.layout = ul } rect
                     return (rects, EmptyWindows n <$> result)

    handleMessage (EmptyWindows n ul) msg =
        do result <- handleMessage ul msg
           return $ EmptyWindows n <$> result
