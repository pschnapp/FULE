{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container
 ( Container(..)
 , LayoutOp
 , runLayoutOp
 , addGuideToLayout
 , nextRenderGroup
 , addComponent
 ) where

import Control.Monad.Trans.State
import Control.Monad.Writer
import Data.Proxy

import FULE.Component
import FULE.Layout


data LayoutOpState
  = LOS
    { builderOf :: LayoutDesign
    , currentRenderGroupOf :: Int
    }

type LayoutOp k m = StateT LayoutOpState (WriterT [ComponentInfo k] m)

runLayoutOp :: (Monad m) => LayoutOp k m () -> m (LayoutDesign, [ComponentInfo k])
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


class (Monad m) => Container c k m where
  -- sadly the `Proxy` has to be used for the heterogenous collections to work
  minWidth :: c -> Proxy k -> m (Maybe Int)
  minHeight :: c -> Proxy k -> m (Maybe Int)
  addToLayout :: c -> Proxy k -> Bounds -> RenderGroup -> LayoutOp k m ()

instance {-# OVERLAPPABLE #-} (Component k m) => Container k k m where
  minWidth k _ = requiredWidth k
  minHeight k _ = requiredHeight k
  addToLayout k _ bounds renderGroup = addComponent $ ComponentInfo bounds k renderGroup

