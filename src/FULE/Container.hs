{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container
-- Description : The @Container@ typeclass.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A typeclass for creating containers of visual content.
module FULE.Container
 ( Container(..)
 , LayoutOp
 , LayoutOpState
 , runLayoutOp
 , addGuideToLayout
 , addGuideConstraintToLayout
 , nextRenderGroup
 ) where

import Control.Monad.Trans.State
import Control.Monad.Writer
import Data.Proxy

import FULE.Component
import FULE.Layout


--------------------------------
-- Layout Operation
--------------------------------

-- | Internal.
data LayoutOpState
  = LOS
    { builderOf :: LayoutDesign
    , currentRenderGroupOf :: Int
    }

-- | An operation that will produce a 'FULE.Layout.LayoutDesign' and a list of
--   components of type @k@ in the monad @m@.
type LayoutOp k m = StateT LayoutOpState (WriterT [ComponentInfo k] m)

-- | Run a 'LayoutOp' to create a 'FULE.Layout.LayoutDesign' and a list of
--   components of type @k@.
runLayoutOp :: (Monad m) => LayoutOp k m () -> m (LayoutDesign, [ComponentInfo k])
runLayoutOp = (toOutput <$>) . runWriterT . (`execStateT` LOS emptyLayoutDesign 0)
  where toOutput (LOS builder _, components) = (builder, components)

-- | Add a Guide to the 'FULE.Layout.LayoutDesign'.
addGuideToLayout :: (Monad m) => GuideSpecification -> LayoutOp k m GuideID
addGuideToLayout r = do
  state <- get
  let (guideID, builder) = addGuide r (builderOf state)
  put state { builderOf = builder }
  return guideID

-- | Add a Guide constraint to the 'FULE.Layout.LayoutDesign'.
addGuideConstraintToLayout
  :: (Monad m)
  => GuideConstraint -> LayoutOp k m ()
addGuideConstraintToLayout constraint = do
  state <- get
  let builder = addGuideConstraint constraint (builderOf state)
  put state { builderOf = builder }

-- | Get the next available render group from the 'LayoutOp' state and advance
--   to the next one internally.
nextRenderGroup :: (Monad m) => LayoutOp k m Int
nextRenderGroup = do
  state <- get
  let renderGroup = currentRenderGroupOf state
  put state { currentRenderGroupOf = renderGroup + 1 }
  return renderGroup


--------------------------------
-- Container
--------------------------------

-- sadly the `Proxy` has to be used for the heterogenous collections to work
--
-- | A typeclass for laying-out a container of type @c@ that holds content of
--   type @k@.
--
--   You'll need to have the @FlexibleInstances@ and @MultiParamTypeClasses@
--   language extensions enabled to use this and you may wish to have your
--   instance use the @{-# OVERLAPS #-}@ or @{-# OVERLAPPING #-}@ pragmas.
class (Monad m) => Container c k m where
  -- | Get the minimum /width/ required for display of the container @c@.
  minWidth :: c -> Proxy k -> m (Maybe Int)
  -- | Get the minimum /height/ required for display of the container @c@.
  minHeight :: c -> Proxy k -> m (Maybe Int)
  -- | Add the container @c@ and its contents @k@ to a 'FULE.Layout.LayoutDesign'.
  addToLayout :: c -> Proxy k -> Bounds -> RenderGroup -> LayoutOp k m ()

instance {-# OVERLAPPABLE #-} (Component k m) => Container k k m where
  minWidth k _ = requiredWidth k
  minHeight k _ = requiredHeight k
  addToLayout k _ bounds renderGroup = tell [ComponentInfo bounds k renderGroup]

