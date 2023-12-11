-- |
-- Module      : FULE.LayoutOp
-- Description : @LayoutDesign@ creation helpers.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- Operations for constructing a 'FULE.Layout.LayoutDesign'.
module FULE.LayoutOp
 ( LayoutOp
 , LayoutOpState
 , runLayoutOp
 , addGuideToLayout
 , addGuideConstraintToLayout
 , nextRenderGroup
 ) where

import Control.Monad.Trans.State
import Control.Monad.Writer

import FULE.Component
import FULE.Layout


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


