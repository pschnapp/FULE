{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Array
 ( ArrayM
 , Array
 , arrayedHoriz
 , arrayedVert
 ) where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy

import FULE.Component
import FULE.Container
import FULE.Container.Item
import FULE.Internal.Direction
import FULE.Internal.Util
import FULE.Layout


data ArrayM m k
  = Array
    { horizPaddingOf :: Int
    , vertPaddingOf :: Int
    , directionOf :: Direction
    , itemsOf :: [ItemM m k]
    }

type Array = ArrayM Identity

-- NOTE: no padding is added when there are no items to display
instance (Monad m) => Container (ArrayM m k) k m where
  minWidth (Array h v d is) proxy = case d of
    Horizontal -> do
      let padding = (length is + 1) * h
      fmap (+ padding) . getTotalSize <$> mapM (`minWidth` proxy) is
    Vertical -> do
      let padding = 2 * h
      fmap (+ padding) . getMaxSize <$> mapM (`minWidth` proxy) is
  minHeight (Array h v d is) proxy = case d of
    Horizontal -> do
      let padding = 2 * v
      fmap (+ padding) . getMaxSize <$> mapM (`minHeight` proxy) is
    Vertical -> do
      let padding = (length is + 1) * v
      fmap (+ padding) . getTotalSize <$> mapM (`minHeight` proxy) is
  addToLayout (Array h v d is) proxy bounds renderGroup = unless (null is) do
    let (refBoundingGuide, getRefBoundingGuide) = case d of
          Horizontal -> (leftOf bounds, rightOf)
          Vertical -> (topOf bounds, bottomOf)
    alignmentGuide <- case d of
      Horizontal -> addGuideToLayout $ Relative v (topOf bounds) Asymmetric
      Vertical -> addGuideToLayout $ Relative h (leftOf bounds) Asymmetric
    loopingWith refBoundingGuide is $ \refBoundingGuide i -> do
      bounds <- makeBounds h v d alignmentGuide refBoundingGuide i proxy
      addToLayout i proxy bounds renderGroup
      return (getRefBoundingGuide bounds)

loopingWith :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
loopingWith b as f = void (foldlM f b as)

makeBounds
  :: (Monad m)
  => Int -> Int -> Direction -> GuideID -> GuideID -> ItemM m k -> Proxy k
  -> LayoutOp k m Bounds
makeBounds horiz vert dir alignmentGuide refBoundingGuide i proxy = do
  width <- fmap (fromMaybe 0) . lift . lift $ minWidth i proxy
  height <- fmap (fromMaybe 0) . lift . lift $ minHeight i proxy
  case dir of
    Horizontal -> do
      boundingGuide <- addGuideToLayout $ Relative horiz refBoundingGuide Asymmetric
      right <- addGuideToLayout $ Relative width boundingGuide Asymmetric
      bottom <- addGuideToLayout $ Relative height alignmentGuide Asymmetric
      return (Bounds alignmentGuide boundingGuide right bottom)
    Vertical -> do
      boundingGuide <- addGuideToLayout $ Relative vert refBoundingGuide Asymmetric
      right <- addGuideToLayout $ Relative width alignmentGuide Asymmetric
      bottom <- addGuideToLayout $ Relative height boundingGuide Asymmetric
      return (Bounds boundingGuide alignmentGuide right bottom)

arrayedHoriz :: Int -> Int -> [ItemM m k] -> ArrayM m k
arrayedHoriz horiz vert = arrayed horiz vert Horizontal

arrayedVert :: Int -> Int -> [ItemM m k] -> ArrayM m k
arrayedVert horiz vert = arrayed horiz vert Vertical

arrayed :: Int -> Int -> Direction -> [ItemM m k] -> ArrayM m k
arrayed horiz vert = Array (max 0 horiz) (max 0 vert)

