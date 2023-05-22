{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container
 ( Container(..)
 , Bounds(..)
 , RenderGroup
 , Component(..)
 , HasBoundingGuides(..)
 , LayoutOp
 , runLayoutOp
 , addGuideToLayout
 , nextRenderGroup
 , addComponent
 ) where

import Control.Monad.Trans.State
import Control.Monad.Writer
import Data.Proxy

import FULE.Layout


data Bounds
  = Bounds
    { topOf :: GuideID
    , leftOf :: GuideID
    , rightOf :: GuideID
    , bottomOf :: GuideID
    }
  deriving (Show)


type RenderGroup = Maybe Int


data Component k
  = Component
    { boundsOf :: Bounds
    , componentOf :: k
    , renderGroupOf :: RenderGroup
    }
  deriving (Functor, Show)


class HasBoundingGuides a where
  boundingGuidesFor :: Layout -> a -> [Int]

instance HasBoundingGuides Bounds where
  boundingGuidesFor layout (Bounds t l r b) =
    getGuides [t, l, r, b] layout

instance HasBoundingGuides (Component k) where
  boundingGuidesFor layout component =
    boundingGuidesFor layout (boundsOf component)


data LayoutOpState
  = LOS
    { builderOf :: LayoutDesign
    , currentRenderGroupOf :: Int
    }

type LayoutOp k m = StateT LayoutOpState (WriterT [Component k] m)

runLayoutOp :: (Monad m) => LayoutOp k m () -> m (LayoutDesign, [Component k])
runLayoutOp = (toOutput <$>) . runWriterT . (`execStateT` LOS makeDesign 0)
  where toOutput (LOS builder _, components) = (builder, components)


addGuideToLayout :: (Monad m) => Relationship -> LayoutOp k m GuideID
addGuideToLayout r = do
  state <- get
  let (guideID, builder) = addGuide r (builderOf state)
  put state { builderOf = builder }
  return guideID

nextRenderGroup :: (Monad m) => LayoutOp k m Int
nextRenderGroup = do
  state <- get
  let renderGroup = currentRenderGroupOf state
  put state { currentRenderGroupOf = renderGroup + 1 }
  return renderGroup

addComponent :: (MonadWriter [a] m) => a -> m ()
addComponent p = tell [p]


class Container c k where
  -- sadly the `Proxy` has to be used for the heterogenous collections to work
  requiredWidth :: (Monad m) => c -> Proxy k -> m (Maybe Int)
  requiredHeight :: (Monad m) => c -> Proxy k -> m (Maybe Int)
  addToLayout :: (Monad m) => c -> Proxy k -> Bounds -> RenderGroup -> LayoutOp k m ()

instance Container k k where
  requiredWidth _ _ = return Nothing
  requiredHeight _ _ = return Nothing
  addToLayout k _ bounds renderGroup = addComponent $ Component bounds k renderGroup

