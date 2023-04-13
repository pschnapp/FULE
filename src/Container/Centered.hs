module Container.Centered
 ( Centered
 , centeredHoriz
 , centeredVert
 , centered
 ) where

import Control.Monad.Trans.State
import Data.Proxy

import Container
import Layout


data Centering = Both | Horizontal | Vertical

data Centered c = Centered Centering c

instance (Container c k) => Container (Centered c) k where
  requiredWidth (Centered _ c) = requiredWidth c
  requiredHeight (Centered _ c) = requiredHeight c
  addToLayout (Centered centering c) bounds renderGroup =
    let proxy = Proxy :: Proxy k
    in case (centering, requiredWidth c proxy, requiredHeight c proxy) of
      (Both, Just w, Just h) -> do
        (top, bottom) <- makeCenteringVerticallyGuides h bounds
        (left, right) <- makeCenteringHorizontallyGuides w bounds
        let bounds' = Bounds top left right bottom
        addToLayout c bounds' renderGroup
      (Horizontal, Just w, _) -> do
        (left, right) <- makeCenteringHorizontallyGuides w bounds
        let bounds' = bounds { leftOf = left, rightOf = right }
        addToLayout c bounds' renderGroup
      (Vertical, _, Just h) -> do
        (top, bottom) <- makeCenteringVerticallyGuides h bounds
        let bounds' = bounds { topOf = top, bottomOf = bottom }
        addToLayout c bounds' renderGroup
      _ -> addToLayout c bounds renderGroup

-- NOTE `horiz` and `vert` here refer to the centering _guides_ --
-- the vertical centering guide is for centering horizontally and vice-versa
makeCenteringHorizontallyGuides :: Int -> Bounds -> LayoutOp k (GuideID, GuideID)
makeCenteringHorizontallyGuides w bounds = do
  vert <- addGuideToLayout $ Between (leftOf bounds, 0.5) (rightOf bounds, 0.5)
  left <- addGuideToLayout $ Relative (-1 * (w `div` 2)) vert Asymmetric
  right <- addGuideToLayout $ Relative w left Symmetric
  return (left, right)

makeCenteringVerticallyGuides :: Int -> Bounds -> LayoutOp k (GuideID, GuideID)
makeCenteringVerticallyGuides h bounds = do
  horiz <- addGuideToLayout $ Between (topOf bounds, 0.5) (bottomOf bounds, 0.5)
  top <- addGuideToLayout $ Relative (-1 * (h `div` 2)) horiz Asymmetric
  bottom <- addGuideToLayout $ Relative h top Symmetric
  return (top, bottom)

centeredHoriz :: c -> Centered c
centeredHoriz = Centered Horizontal

centeredVert :: c -> Centered c
centeredVert = Centered Vertical

centered :: c -> Centered c
centered = Centered Both

