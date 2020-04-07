{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Layout.OptionalTitleBar
    ( module Layout.OptionalTitleBar
    ) where

import           XMonad
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet              as W

import           XMonad.Layout.Decoration

import           Basic

import           Control.Monad.Reader
import           Data.Reflection
import           Data.Proxy



data OptionalDecoration x ds a = OD x (ds a)
    deriving (Show, Read, Typeable)

class (Read a, Show a) => WindowFilter a where
    tests :: a -> [Query Bool]
    describeFilter :: a -> String
    describeFilter _ = ""

instance (WindowFilter x, DecorationStyle ds Window) => DecorationStyle (OptionalDecoration x ds) Window where
    describeDeco (OD fil ds) = "Optional" ++ describeFilter fil ++ " " ++ describeDeco ds

    shrink (OD _ ds) = shrink ds

    decorationEventHook (OD _ ds) = decorationEventHook ds
    decorationCatchClicksHook (OD _ ds) = decorationCatchClicksHook ds
    decorationWhileDraggingHook (OD _ ds) = decorationWhileDraggingHook ds
    decorationAfterDraggingHook (OD _ ds) = decorationAfterDraggingHook ds

    decorate (OD fil ds) w h r s wrs (a,wr) =
        do results <- mapM (flip runQuery a) (tests fil)
           if or results
           then decorate ds w h r s wrs (a,wr)
           else return Nothing

data TitleOverlay a = TitleOverlay 
   deriving (Show, Read)

instance Eq a => DecorationStyle TitleOverlay a where
    describeDeco _ = "TitleOverlay"
    shrink TitleOverlay (Rectangle _ _ _ dh) r@(Rectangle x y w h) = r
    pureDecoration TitleOverlay wh ht _ s _ (w,Rectangle x y wid _) =
        if isInStack s w
        then Just $ Rectangle x y nwh ht
        else Nothing
            where nwh = min wid wh

isFocus :: Query Bool
isFocus = Query . ReaderT $ \ w -> 
    do s <- gets (W.stack . W.workspace . W.current . windowset)
       case W.focus <$> s of
          Nothing -> return False
          Just id -> return $ id == w


--
--
--data OptionalDecoration x ds a = OD (ds a)
--    deriving (Show, Read, Typeable)
--
--instance (Reifies x [ReaderT a X Bool], DecorationStyle ds a) => DecorationStyle (OptionalDecoration x ds) a where
--    describeDeco (OD ds) = "Optional " ++ describeDeco ds
--    decorate (OD ds) w h r s wrs (a,wr) =
--        do tests <- mapM (flip runReaderT a) (reflect (Proxy::Proxy x))
--           if or tests
--           then decorate ds w h r s wrs (a,wr)
--           else return Nothing
--
--optionalDecoration :: DecorationStyle ds Window => [Query Bool] -> ds Window -> (forall x. Reifies x [ReaderT a X Bool] => OptionalDecoration x ds Window)
--optionalDecoration tests ds = reify tests $ \ x -> OD ds
--
--
--
--data OptionalDecoration ds a = OD
--
--data OptionalTitleBar a = OTB [ReaderT a X Bool]
--    deriving (Show, Read, Typeable)
--
--class OptionalDecoration x a where
--    showTitleWhen :: proxy x -> [ReaderT a X Bool]
--
--instance (OptionalTitleBar x a, Eq a) => DecorationStyle x a where
--    describeDeco _ = "Optional title bar"
--    pureDecoration otb w h r s wrs (a, Rectangle x y wh ww) =
--        if isInStack s a && h < wh && passed a
--        then Just $ Rectangle x y wh h
--        else Nothing
--        where passed w' = any $ map (flip runReaderT w') (showTitleWhen otb)

