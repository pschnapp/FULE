{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Positioned
 ( Positioned
 , topLeft
 , topMiddle
 , topRight
 , middleLeft
 , middleMiddle
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

data Positioned c = Positioned Position c

instance (Container c k m) => Container (Positioned c) k m where
  minWidth (Positioned _ c) = minWidth c
  minHeight (Positioned _ c) = minHeight c
  addToLayout (Positioned position c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    bounds' <- case position of
      TopLeft -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz)
      TopMiddle -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz)
      TopRight -> do
        vert <- adhereTop bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz)
      MiddleLeft -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz)
      MiddleMiddle -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz)
      MiddleRight -> do
        vert <- adhereMiddleVert bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz)
      BottomLeft -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereLeft bounds reqWidth
        return (makeBounds vert horiz)
      BottomMiddle -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereMiddleHoriz bounds reqWidth
        return (makeBounds vert horiz)
      BottomRight -> do
        vert <- adhereBottom bounds reqHeight
        horiz <- adhereRight bounds reqWidth
        return (makeBounds vert horiz)
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

makeBounds (top, bottom) (left, right) =
  Bounds { topOf = top, leftOf = left, rightOf = right, bottomOf = bottom }

topLeft :: c -> Positioned c
topLeft = Positioned TopLeft

topMiddle :: c -> Positioned c
topMiddle = Positioned TopMiddle

topRight :: c -> Positioned c
topRight = Positioned TopRight

middleLeft :: c -> Positioned c
middleLeft = Positioned MiddleLeft

middleMiddle :: c -> Positioned c
middleMiddle = Positioned MiddleMiddle

centered :: c -> Positioned c
centered = middleMiddle

middleRight :: c -> Positioned c
middleRight = Positioned MiddleRight

bottomLeft :: c -> Positioned c
bottomLeft = Positioned BottomLeft

bottomMiddle :: c -> Positioned c
bottomMiddle = Positioned BottomMiddle

bottomRight :: c -> Positioned c
bottomRight = Positioned BottomRight

