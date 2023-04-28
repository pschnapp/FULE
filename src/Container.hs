{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Container
 ( Container(..)
 , Bounds(..)
 , RenderGroup
 , Component(..)
 , LayoutOp
 , runLayoutOp
 , addGuideToLayout
 , nextRenderGroup
 , addComponent
 ) where

import Control.Monad.Trans.State
import Control.Monad.Writer
import Data.Proxy

import Layout


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
  deriving (Show)


data LayoutOpState
  = LOS
    { builderOf :: LayoutDesign
    , currentRenderGroupOf :: Int
    }

type LayoutOp k = StateT LayoutOpState (Writer [Component k])

runLayoutOp :: LayoutOp k () -> (LayoutDesign, [Component k])
runLayoutOp = toOutput . runWriter . (`execStateT` LOS makeDesign 0)
  where toOutput (LOS builder _, components) = (builder, components)


addGuideToLayout :: Relationship -> LayoutOp k GuideID
addGuideToLayout r = do
  state <- get
  let (guideID, builder) = addGuide r (builderOf state)
  put state { builderOf = builder }
  return guideID

nextRenderGroup :: LayoutOp k Int
nextRenderGroup = do
  state <- get
  let renderGroup = currentRenderGroupOf state
  put state { currentRenderGroupOf = renderGroup + 1 }
  return renderGroup

addComponent :: MonadWriter [a] m => a -> m ()
addComponent p = tell [p]


class Container c k where
  -- sadly the `Proxy` has to be used for the heterogenous collections to work
  requiredWidth :: c -> Proxy k -> Maybe Int
  requiredHeight :: c -> Proxy k -> Maybe Int
  addToLayout :: c -> Proxy k -> Bounds -> RenderGroup -> LayoutOp k ()

instance Container k k where
  requiredWidth _ _ = Nothing
  requiredHeight _ _ = Nothing
  addToLayout k _ bounds renderGroup = addComponent $ Component bounds k renderGroup

