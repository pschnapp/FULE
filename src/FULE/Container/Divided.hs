{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container.Divided
-- Description : The @Divided@ Container.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A 'FULE.Container.Container' that is divided in half, horizontally or vertically.
module FULE.Container.Divided
 ( Divided
 , BarControlGen
 , Dynamics
 , dynamic
 , static
 , sizedTop
 , sizedLeft
 , sizedRight
 , sizedBottom
 ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Container.Config
import FULE.Internal.Util
import FULE.Layout
import FULE.LayoutOp


data SizedSide = SizedTop | SizedLeft | SizedRight | SizedBottom


-- | Type of a function to produce a 'FULE.Component.Component' for controlling
--   the resize bar, thus leaving the mechanics and presentation up to you.
type BarControlGen b
  = Orientation
  -- ^ The orientation of the resize bar; a @Horizontal@ orientation would call
  --   for vertical movement, so should be paired with watching for changes in
  --   the @y@ axis, and likewise with @Vertical@ and the @x@ axis.
  -> GuideID
  -- ^ A Guide associated with the resize bar that should be updated with a
  --   delta when the bar is moved.
  -> b -- ^ The bar component to be added to the layout.

-- | A specification of whether the sized portion of the container should be
--   resizable and how the resize bar, if any, should be sized and controlled.
data Dynamics b
  = Dynamic
    { barGenOf :: BarControlGen b
    , barSizeOf :: Int
    }
  | Static

barSizeFor :: Dynamics b -> Maybe Int
barSizeFor (Dynamic _ s) = Just s
barSizeFor Static = Nothing

-- | Use a dynamic sizing, with a resize bar, for the sized portion of the
--   container.
dynamic
  :: BarControlGen b
  -- ^ A function to generate a @Component@ to use for the resize bar.
  -> Int
  -- ^ The thickness of the resize bar @Component@ in the direction of travel.
  -> Dynamics b
dynamic genBar size = Dynamic genBar (max 0 size)

-- | Use a static size for the sized portion of the container.
static :: Dynamics b
static = Static


-- | A container divided (horizontally or vertically) into two parts with one
--   of the parts having a set size (height or width) and the other part
--   resizing dynamically.
--
--   When configured to be resizable, the resize bar's size is not included in
--   the size of the sized content during layout but is treated as an additional
--   size to be considered.
data Divided s b u
  = Divided
    { sizedSideOf :: SizedSide
    , sizeOf :: SizedContentSize Int
    , dynamicsOf :: Dynamics b
    , sizedContentOf :: s
    , unconstrainedContentOf :: u
    }

instance (Container s b m, Container u b m) => Container (Divided s b u) b m where
  minWidth divided proxy = do
    sizedWidth <- minWidth (sizedContentOf divided) proxy
    let barSize = barSizeFor (dynamicsOf divided)
    let sizedWidth' = getTotalSize [sizeOf divided <|> sizedWidth, barSize]
    case sizedSideOf divided of
      SizedLeft  -> return sizedWidth'
      SizedRight -> return sizedWidth'
      _          -> return sizedWidth
  minHeight divided proxy = do
    sizedHeight <- minHeight (sizedContentOf divided) proxy
    let barSize = barSizeFor (dynamicsOf divided)
    let sizedHeight' = getTotalSize [sizeOf divided <|> sizedHeight, barSize]
    case sizedSideOf divided of
      SizedTop    -> return sizedHeight'
      SizedBottom -> return sizedHeight'
      _           -> return sizedHeight
  addToLayout divided proxy bounds renderGroup =
    case sizedSideOf divided of
      SizedTop -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { topOf = g }
        , getUnconOuterOf = bottomOf
        , setSizedInnerOf = \g b -> b { bottomOf = g }
        , getSizedOuterOf = topOf
        , multiplierOf = 1
        , orientationOf = Horizontal
        }
      SizedLeft -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { leftOf = g }
        , getUnconOuterOf = rightOf
        , setSizedInnerOf = \g b -> b { rightOf = g }
        , getSizedOuterOf = leftOf
        , multiplierOf = 1
        , orientationOf = Vertical
        }
      SizedRight -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { rightOf = g }
        , getUnconOuterOf = leftOf
        , setSizedInnerOf = \g b -> b { leftOf = g }
        , getSizedOuterOf = rightOf
        , multiplierOf = -1
        , orientationOf = Vertical
        }
      SizedBottom -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { bottomOf = g }
        , getUnconOuterOf = topOf
        , setSizedInnerOf = \g b -> b { topOf = g }
        , getSizedOuterOf = bottomOf
        , multiplierOf = -1
        , orientationOf = Horizontal
        }


data DivisionConfig
  = DivisionConfig
    { setUnconInnerOf :: GuideID -> Bounds -> Bounds
    , getUnconOuterOf :: Bounds -> GuideID
    , setSizedInnerOf :: GuideID -> Bounds -> Bounds
    , getSizedOuterOf :: Bounds -> GuideID
    , multiplierOf :: Int
    , orientationOf :: Orientation
    }

makeDivided
 :: (Container s b m, Container u b m)
 => Divided s b u -> Proxy b -> Bounds -> RenderGroup -> DivisionConfig -> LayoutOp b m ()
makeDivided divided proxy bounds renderGroup config = do
  -- sized
  dim <- case orientation of
    -- a Horizontal `orientation` means we're split horizontally so should get the height
    -- and likewise for Vertical and width
    Horizontal -> lift . lift $ minHeight sized proxy
    Vertical -> lift . lift $ minWidth sized proxy
  let size' = m * fromMaybe 0 (size <|> dim)
  sizedInner <- addGuideToLayout $ Relative size' (getSizedOuter bounds) Asymmetric
  addToLayout sized proxy (setSizedInner sizedInner bounds) renderGroup
  -- bar
  unconstrainedInner <- case dynamics of
    Dynamic genBar barSize -> do
      barUncon <- addGuideToLayout $ Relative (m * barSize) sizedInner Symmetric
      -- yes the 'sized' and 'unconstrained' are supposed to be mixed here:
      let barBounds = setSizedInner barUncon . setUnconInner sizedInner $ bounds
      addToLayout (genBar orientation sizedInner) proxy barBounds renderGroup
      addGuideConstraintToLayout $ barUncon `unconConstraint` getUnconOuter bounds
      addGuideConstraintToLayout $ sizedInner `sizedConstraint` getSizedOuter bounds
      return barUncon
    Static -> return sizedInner
  -- unconstrained
  addToLayout unconstrained proxy (setUnconInner unconstrainedInner bounds) renderGroup
  where
    Divided
      { sizeOf = size
      , dynamicsOf = dynamics
      , sizedContentOf = sized
      , unconstrainedContentOf = unconstrained
      } = divided
    DivisionConfig
      { setUnconInnerOf = setUnconInner
      , getUnconOuterOf = getUnconOuter
      , setSizedInnerOf = setSizedInner
      , getSizedOuterOf = getSizedOuter
      , multiplierOf = m
      , orientationOf = orientation
      } = config
    (unconConstraint, sizedConstraint) = if m == 1 then (LTE, GTE) else (GTE, LTE)


-- | Create a 'Divided' container with the top portion having a particular size.
sizedTop
  :: SizedContentSize Int -- ^ The size of the sized content.
  -> Dynamics b -- ^ The dynamics of the @Divided@ container.
  -> s -- ^ The sized content.
  -> u -- ^ The dynamic content.
  -> Divided s b u
sizedTop = Divided SizedTop . fmap (max 0)

-- | Create a 'Divided' container with the left portion having a particular size.
sizedLeft
  :: SizedContentSize Int -- ^ The size of the sized content.
  -> Dynamics b -- ^ The dynamics of the @Divided@ container.
  -> s -- ^ The sized content.
  -> u -- ^ The dynamic content.
  -> Divided s b u
sizedLeft = Divided SizedLeft . fmap (max 0)

-- | Create a 'Divided' container with the right portion having a particular size.
sizedRight
  :: SizedContentSize Int -- ^ The size of the sized content.
  -> Dynamics b -- ^ The dynamics of the @Divided@ container.
  -> s -- ^ The sized content.
  -> u -- ^ The dynamic content.
  -> Divided s b u
sizedRight = Divided SizedRight . fmap (max 0)

-- | Create a 'Divided' container with the bottom portion having a particular size.
sizedBottom
  :: SizedContentSize Int -- ^ The size of the sized content.
  -> Dynamics b -- ^ The dynamics of the @Divided@ container.
  -> s -- ^ The sized content.
  -> u -- ^ The dynamic content.
  -> Divided s b u
sizedBottom = Divided SizedBottom . fmap (max 0)

