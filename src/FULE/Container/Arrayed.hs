{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Arrayed
 ( ArrayedM
 , Arrayed
 , arrayedHoriz
 , arrayedVert
 , Padding
 , padding
 , noPadding
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
import FULE.Internal.Util
import FULE.Layout
import FULE.Orientation


type Padding = (Int, Int)

padding :: Int -> Int -> Padding
padding horiz vert = (horiz, vert)

noPadding :: Padding
noPadding = (0, 0)


data ArrayedM m k
  = Arrayed
    { horizPaddingOf :: Int
    , vertPaddingOf :: Int
    , orientationOf :: Orientation
    , itemsOf :: [ItemM m k]
    }

type Arrayed = ArrayedM Identity

-- NOTE: no padding is added when there are no items to display
instance (Monad m) => Container (ArrayedM m k) k m where
  minWidth (Arrayed h v o is) proxy = case o of
    Horizontal -> do
      let padding = (length is + 1) * h
      fmap (+ padding) . getTotalSize <$> mapM (`minWidth` proxy) is
    Vertical -> do
      let padding = 2 * h
      fmap (+ padding) . getMaxSize <$> mapM (`minWidth` proxy) is
  minHeight (Arrayed h v o is) proxy = case o of
    Horizontal -> do
      let padding = 2 * v
      fmap (+ padding) . getMaxSize <$> mapM (`minHeight` proxy) is
    Vertical -> do
      let padding = (length is + 1) * v
      fmap (+ padding) . getTotalSize <$> mapM (`minHeight` proxy) is
  addToLayout (Arrayed h v o is) proxy bounds renderGroup = unless (null is) do
    let (refBoundingGuide, getRefBoundingGuide) = case o of
          Horizontal -> (leftOf bounds, rightOf)
          Vertical -> (topOf bounds, bottomOf)
    alignmentGuide <- case o of
      Horizontal -> addGuideToLayout $ Relative v (topOf bounds) Asymmetric
      Vertical -> addGuideToLayout $ Relative h (leftOf bounds) Asymmetric
    loopingWith refBoundingGuide is $ \refBoundingGuide i -> do
      bounds <- makeBounds h v o alignmentGuide refBoundingGuide i proxy
      addToLayout i proxy bounds renderGroup
      return (getRefBoundingGuide bounds)

loopingWith :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
loopingWith b as f = void (foldlM f b as)

makeBounds
  :: (Monad m)
  => Int -> Int -> Orientation -> GuideID -> GuideID -> ItemM m k -> Proxy k
  -> LayoutOp k m Bounds
makeBounds horiz vert dir alignmentGuide refBoundingGuide i proxy = do
  width <- fmap (fromMaybe 0) . lift . lift $ minWidth i proxy
  height <- fmap (fromMaybe 0) . lift . lift $ minHeight i proxy
  case dir of
    Horizontal -> do
      boundingGuide <-
        if horiz /= 0
        then addGuideToLayout $ Relative horiz refBoundingGuide Asymmetric
        else return refBoundingGuide
      right <- addGuideToLayout $ Relative width boundingGuide Asymmetric
      bottom <- addGuideToLayout $ Relative height alignmentGuide Asymmetric
      return (Bounds alignmentGuide boundingGuide right bottom)
    Vertical -> do
      boundingGuide <-
        if vert /= 0
        then addGuideToLayout $ Relative vert refBoundingGuide Asymmetric
        else return refBoundingGuide
      right <- addGuideToLayout $ Relative width alignmentGuide Asymmetric
      bottom <- addGuideToLayout $ Relative height boundingGuide Asymmetric
      return (Bounds boundingGuide alignmentGuide right bottom)

arrayedHoriz :: Padding -> [ItemM m k] -> ArrayedM m k
arrayedHoriz padding = arrayed padding Horizontal

arrayedVert :: Padding -> [ItemM m k] -> ArrayedM m k
arrayedVert padding = arrayed padding Vertical

arrayed :: Padding -> Orientation -> [ItemM m k] -> ArrayedM m k
arrayed (horiz, vert) = Arrayed (max 0 horiz) (max 0 vert)

