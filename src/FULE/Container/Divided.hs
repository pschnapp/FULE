{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Divided
 ( Divided
 , Dynamics
 , dynamic
 , static
 , SizedSide
 , sizedTop
 , sizedLeft
 , sizedRight
 , sizedBottom
 , Size
 , sizedTo
 , sizedToContents
 ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Internal.Util
import FULE.Layout
import FULE.Orientation


type Size a = Maybe a

sizedTo :: a -> Size a
sizedTo = Just

sizedToContents :: Size a
sizedToContents = Nothing


data SizedSide = SizedTop | SizedLeft | SizedRight | SizedBottom


type BarGenerator b = Orientation -> GuideID -> b

data Dynamics b
  = Dynamic
    { contentsGenOf :: BarGenerator b
    , barSizeOf :: Int
    }
  | Static

barSizeFor :: Dynamics b -> Maybe Int
barSizeFor (Dynamic _ s) = Just s
barSizeFor Static = Nothing

dynamic :: BarGenerator b -> Int -> Dynamics b
dynamic genBar size = Dynamic genBar (max 0 size)

static :: Dynamics b
static = Static


data Divided s b u
  = Divided
    { sizedSideOf :: SizedSide
    , sizeOf :: Size Int
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
      addComponent $ ComponentInfo barBounds (genBar orientation sizedInner) renderGroup
      addConstraintToLayout barUncon unconConstraint (getUnconOuter bounds)
      addConstraintToLayout sizedInner sizedConstraint (getSizedOuter bounds)
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


sizedTop :: Size Int -> Dynamics b -> s -> u -> Divided s b u
sizedTop = Divided SizedTop . fmap (max 0)

sizedLeft :: Size Int -> Dynamics b -> s -> u -> Divided s b u
sizedLeft = Divided SizedLeft . fmap (max 0)

sizedRight :: Size Int -> Dynamics b -> s -> u -> Divided s b u
sizedRight = Divided SizedRight . fmap (max 0)

sizedBottom :: Size Int -> Dynamics b -> s -> u -> Divided s b u
sizedBottom = Divided SizedBottom . fmap (max 0)

