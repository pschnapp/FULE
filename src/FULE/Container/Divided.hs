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
 , sizeTo
 , sizeToContents
 ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Internal.Direction
import FULE.Internal.Util
import FULE.Layout


type Size = Maybe Int

sizeTo :: Int -> Size
sizeTo = Just

sizeToContents :: Size
sizeToContents = Nothing


data SizedSide = SizedTop | SizedLeft | SizedRight | SizedBottom


data Dynamics b
  = Dynamic
    { contentsGenOf :: GuideID -> b
    , barSizeOf :: Int
    }
  | Static

barSizeFor :: Dynamics b -> Maybe Int
barSizeFor (Dynamic _ s) = Just s
barSizeFor Static = Nothing

dynamic :: (GuideID -> b) -> Int -> Dynamics b
dynamic barGen size = Dynamic barGen (max 0 size)

static :: Dynamics b
static = Static


data Divided s b u
  = Divided
    { sizedSideOf :: SizedSide
    , sizeOf :: Size
    , dynamicsOf :: Dynamics b
    , sizedContentOf :: s
    , unconstrainedContentOf :: u
    }

instance (Container s b m, Container u b m) => Container (Divided s b u) b m where
  minWidth divided proxy = do
    sizedWidth <- minWidth (sizedContentOf divided) proxy
    case sizedSideOf divided of
      SizedLeft  -> return $ getTotalSize [sizedWidth, barSizeFor (dynamicsOf divided)]
      SizedRight -> return $ getTotalSize [sizedWidth, barSizeFor (dynamicsOf divided)]
      _          -> return sizedWidth
  minHeight divided proxy = do
    sizedHeight <- minHeight (sizedContentOf divided) proxy
    case sizedSideOf divided of
      SizedTop    -> return $ getTotalSize [sizedHeight, barSizeFor (dynamicsOf divided)]
      SizedBottom -> return $ getTotalSize [sizedHeight, barSizeFor (dynamicsOf divided)]
      _           -> return sizedHeight
  addToLayout divided proxy bounds renderGroup =
    case sizedSideOf divided of
      SizedTop -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { topOf = g }
        , setSizedInnerOf = \g b -> b { bottomOf = g }
        , setSizedOuterOf = topOf
        , multiplierOf = 1
        , directionOf = Horizontal
        }
      SizedLeft -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { leftOf = g }
        , setSizedInnerOf = \g b -> b { rightOf = g }
        , setSizedOuterOf = leftOf
        , multiplierOf = 1
        , directionOf = Vertical
        }
      SizedRight -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { rightOf = g }
        , setSizedInnerOf = \g b -> b { leftOf = g }
        , setSizedOuterOf = rightOf
        , multiplierOf = -1
        , directionOf = Vertical
        }
      SizedBottom -> makeDivided divided proxy bounds renderGroup
        DivisionConfig
        { setUnconInnerOf = \g b -> b { bottomOf = g }
        , setSizedInnerOf = \g b -> b { topOf = g }
        , setSizedOuterOf = bottomOf
        , multiplierOf = -1
        , directionOf = Horizontal
        }


data DivisionConfig
  = DivisionConfig
    { setUnconInnerOf :: GuideID -> Bounds -> Bounds
    , setSizedInnerOf :: GuideID -> Bounds -> Bounds
    , setSizedOuterOf :: Bounds -> GuideID
    , multiplierOf :: Int
    , directionOf :: Direction
    }

makeDivided
 :: (Container s b m, Container u b m)
 => Divided s b u -> Proxy b -> Bounds -> RenderGroup -> DivisionConfig -> LayoutOp b m ()
makeDivided divided proxy bounds renderGroup config = do
  -- sized
  dim <- case dir of
    -- a Horizontal `dir` means we're split horizontally so should get the height
    -- and likewise for Vertical and width
    Horizontal -> lift . lift $ requiredHeight sized
    Vertical -> lift . lift $ requiredWidth sized
  let size' = m * fromMaybe 0 (size <|> dim)
  sizedInner <- addGuideToLayout $ Relative size' (getSizedOuter bounds) Asymmetric
  addToLayout sized proxy (setSizedInner sizedInner bounds) renderGroup
  -- bar
  unconstrainedInner <- case dynamics of
    Dynamic barGen barSize -> do
      barUncon <- addGuideToLayout $ Relative (m * barSize) sizedInner Symmetric
      -- yes the 'sized' and 'unconstrained' are supposed to be mixed here:
      let barBounds = setSizedInner barUncon . setUnconInner sizedInner $ bounds
      addComponent $ ComponentInfo barBounds (barGen sizedInner) renderGroup
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
      , setSizedInnerOf = setSizedInner
      , setSizedOuterOf = getSizedOuter
      , multiplierOf = m
      , directionOf = dir
      } = config


sizedTop :: Size -> Dynamics b -> s -> u -> Divided s b u
sizedTop = Divided SizedTop . fmap (max 0)

sizedLeft :: Size -> Dynamics b -> s -> u -> Divided s b u
sizedLeft = Divided SizedLeft . fmap (max 0)

sizedRight :: Size -> Dynamics b -> s -> u -> Divided s b u
sizedRight = Divided SizedRight . fmap (max 0)

sizedBottom :: Size -> Dynamics b -> s -> u -> Divided s b u
sizedBottom = Divided SizedBottom . fmap (max 0)

