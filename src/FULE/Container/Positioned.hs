{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container.Positioned
-- Description : The @Positioned@ Container.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A 'FULE.Container.Container' to position content relative to its parent
-- container, including in the center.
module FULE.Container.Positioned
 ( Positioned
 , topLeft
 , topMiddle
 , topRight
 , middleLeft
 , centered
 , middleRight
 , bottomLeft
 , bottomMiddle
 , bottomRight
 ) where

import Control.Monad.Trans.Class
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Layout
import FULE.LayoutOp


data Position
  = TopLeft
  | TopMiddle
  | TopRight
  | MiddleLeft
  | MiddleMiddle
  | MiddleRight
  | BottomLeft
  | BottomMiddle
  | BottomRight

-- | A container for positioning content within a larger container in one of
--   nine positions relative to the parent. Relative positioning will adjust
--   to keep up with changes to the size of the parent container.
data Positioned c = Positioned Position c

instance (Container c k m) => Container (Positioned c) k m where
  minWidth (Positioned _ c) = minWidth c
  minHeight (Positioned _ c) = minHeight c
  addToLayout (Positioned position c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    let clipping = clippingOf bounds
    bounds' <- case position of
      TopLeft -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz clipping)
      TopMiddle -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz clipping)
      TopRight -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz clipping)
      MiddleLeft -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz clipping)
      MiddleMiddle -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz clipping)
      MiddleRight -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz clipping)
      BottomLeft -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz clipping)
      BottomMiddle -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz clipping)
      BottomRight -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz clipping)
    addToLayout c proxy bounds' renderGroup

adhereTop :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereTop bounds Nothing = return (topOf bounds, bottomOf bounds)
adhereTop (Bounds { topOf = top }) (Just h) = do
  bottom <- addGuideToLayout $ Relative h top Asymmetric
  return (top, bottom)

adhereMiddleVert :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereMiddleVert bounds Nothing = return (topOf bounds, bottomOf bounds)
adhereMiddleVert bounds (Just h) = do
  horiz <- addGuideToLayout $ Between (topOf bounds, 0.5) (bottomOf bounds, 0.5)
  top <- addGuideToLayout $ Relative (-1 * (h `div` 2)) horiz Asymmetric
  bottom <- addGuideToLayout $ Relative h top Symmetric
  return (top, bottom)

adhereBottom :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereBottom bounds Nothing = return (topOf bounds, bottomOf bounds)
adhereBototm (Bounds { bottomOf = bottom }) (Just h) = do
  top <- addGuideToLayout $ Relative (-1 * h) bottom Asymmetric
  return (top, bottom)

adhereLeft :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereLeft bounds Nothing = return (leftOf bounds, rightOf bounds)
adhereLeft (Bounds { leftOf = left }) (Just w) = do
  right <- addGuideToLayout $ Relative w left Asymmetric
  return (left, right)

adhereMiddleHoriz :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereMiddleHoriz bounds Nothing = return (leftOf bounds, rightOf bounds)
adhereMiddleHoriz bounds (Just w) = do
  vert <- addGuideToLayout $ Between (leftOf bounds, 0.5) (rightOf bounds, 0.5)
  left <- addGuideToLayout $ Relative (-1 * (w `div` 2)) vert Asymmetric
  right <- addGuideToLayout $ Relative w left Symmetric
  return (left, right)

adhereRight :: (Monad m) => Bounds -> Maybe Int -> LayoutOp k m (GuideID, GuideID)
adhereRight bounds Nothing = return (leftOf bounds, rightOf bounds)
adhereRight (Bounds { rightOf = right }) (Just w) = do
  left <- addGuideToLayout $ Relative (-1 * w) right Asymmetric
  return (left, right)

makeBounds :: (GuideID, GuideID) -> (GuideID, GuideID) -> Maybe Bounds -> Bounds
makeBounds (top, bottom) (left, right) clipping =
  Bounds
  { topOf = top
  , leftOf = left
  , rightOf = right
  , bottomOf = bottom
  , clippingOf = clipping
  }

-- | Position content in the /top-left/ corner of its parent container.
topLeft :: c -> Positioned c
topLeft = Positioned TopLeft

-- | Position content in the middle of the /top/ side of its parent container.
topMiddle :: c -> Positioned c
topMiddle = Positioned TopMiddle

-- | Position content in the /top-right/ corner of its parent container.
topRight :: c -> Positioned c
topRight = Positioned TopRight

-- | Position content in the middle of the /left/ side of its parent container.
middleLeft :: c -> Positioned c
middleLeft = Positioned MiddleLeft

-- | Position content in the very center of its parent container.
centered :: c -> Positioned c
centered = Positioned MiddleMiddle

-- | Position content in the middle of the /right/ side of its parent container.
middleRight :: c -> Positioned c
middleRight = Positioned MiddleRight

-- | Position content in the /bottom-left/ corner of its parent container.
bottomLeft :: c -> Positioned c
bottomLeft = Positioned BottomLeft

-- | Position content in the middle of the /bottom/ side of its parent container.
bottomMiddle :: c -> Positioned c
bottomMiddle = Positioned BottomMiddle

-- | Position content in the /bottom-right/ corner of its parent container.
bottomRight :: c -> Positioned c
bottomRight = Positioned BottomRight

