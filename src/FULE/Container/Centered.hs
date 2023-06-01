{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Centered
 ( Centered
 , centeredHoriz
 , centeredVert
 , centered
 ) where

import Control.Monad.Trans.Class
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Internal.Direction
import FULE.Layout


data Centered c = Centered (Maybe Direction) c

instance (Container c k m) => Container (Centered c) k m where
  minWidth (Centered _ c) = minWidth c
  minHeight (Centered _ c) = minHeight c
  addToLayout (Centered centering c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    case (centering, reqWidth, reqHeight) of
      (Nothing, Just w, Just h) -> do
        (top, bottom) <- makeCenteringVerticallyGuides h bounds
        (left, right) <- makeCenteringHorizontallyGuides w bounds
        let bounds' = Bounds top left right bottom
        addToLayout c proxy bounds' renderGroup
      (Just Horizontal, Just w, _) -> do
        (left, right) <- makeCenteringHorizontallyGuides w bounds
        let bounds' = bounds { leftOf = left, rightOf = right }
        addToLayout c proxy bounds' renderGroup
      (Just Vertical, _, Just h) -> do
        (top, bottom) <- makeCenteringVerticallyGuides h bounds
        let bounds' = bounds { topOf = top, bottomOf = bottom }
        addToLayout c proxy bounds' renderGroup
      _ -> addToLayout c proxy bounds renderGroup

-- NOTE `horiz` and `vert` here refer to the centering _guides_ --
-- the vertical centering guide is for centering horizontally and vice-versa

makeCenteringHorizontallyGuides
  :: (Monad m)
  => Int -> Bounds -> LayoutOp k m (GuideID, GuideID)
makeCenteringHorizontallyGuides w bounds = do
  vert <- addGuideToLayout $ Between (leftOf bounds, 0.5) (rightOf bounds, 0.5)
  left <- addGuideToLayout $ Relative (-1 * (w `div` 2)) vert Asymmetric
  right <- addGuideToLayout $ Relative w left Symmetric
  return (left, right)

makeCenteringVerticallyGuides
  :: (Monad m)
  => Int -> Bounds -> LayoutOp k m (GuideID, GuideID)
makeCenteringVerticallyGuides h bounds = do
  horiz <- addGuideToLayout $ Between (topOf bounds, 0.5) (bottomOf bounds, 0.5)
  top <- addGuideToLayout $ Relative (-1 * (h `div` 2)) horiz Asymmetric
  bottom <- addGuideToLayout $ Relative h top Symmetric
  return (top, bottom)

centeredHoriz :: c -> Centered c
centeredHoriz = Centered (Just Horizontal)

centeredVert :: c -> Centered c
centeredVert = Centered (Just Vertical)

centered :: c -> Centered c
centered = Centered Nothing

