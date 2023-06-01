{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Overflowed
 ( Overflowed
 , overflowedHoriz
 , overflowedVert
 , overflowed
 ) where

import Control.Monad.Trans.Class
import Data.Maybe

import FULE.Component
import FULE.Container
import FULE.Internal.Direction
import FULE.Layout


data Overflowed c = Overflowed (Maybe Direction) c

instance (Container c k m) => Container (Overflowed c) k m where
  minWidth (Overflowed dir c) proxy = case dir of
    Just Vertical -> minWidth c proxy
    _ -> return Nothing
  minHeight (Overflowed dir c) proxy = case dir of
    Just Horizontal -> minHeight c proxy
    _ -> return Nothing
  addToLayout (Overflowed dir c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    let Bounds t l r b = bounds
    right <- addGuideToLayout $ Relative (fromMaybe 0 reqWidth) l Asymmetric
    bottom <- addGuideToLayout $ Relative (fromMaybe 0 reqHeight) t Asymmetric
    let bounds = Bounds t l right bottom
    addToLayout c proxy bounds renderGroup

overflowedHoriz :: c -> Overflowed c
overflowedHoriz = Overflowed (Just Horizontal)

overflowedVert :: c -> Overflowed c
overflowedVert = Overflowed (Just Vertical)

overflowed :: c -> Overflowed c
overflowed = Overflowed Nothing

