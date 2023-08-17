{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Overflow
 ( Overflow
 , overflowHoriz
 , overflowVert
 , overflow
 ) where

import Control.Monad.Trans.Class
import Data.Maybe

import FULE.Common
import FULE.Component
import FULE.Container
import FULE.Layout


data Overflow c = Overflow (Maybe Orientation) c

instance (Container c k m) => Container (Overflow c) k m where
  minWidth (Overflow o c) proxy = case o of
    Just Vertical -> minWidth c proxy
    _ -> return Nothing
  minHeight (Overflow o c) proxy = case o of
    Just Horizontal -> minHeight c proxy
    _ -> return Nothing
  addToLayout (Overflow _ c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    let Bounds t l r b = bounds
    right <- addGuideToLayout $ Relative (fromMaybe 0 reqWidth) l Asymmetric
    bottom <- addGuideToLayout $ Relative (fromMaybe 0 reqHeight) t Asymmetric
    let bounds = Bounds t l right bottom
    addToLayout c proxy bounds renderGroup

overflowHoriz :: c -> Overflow c
overflowHoriz = Overflow (Just Horizontal)

overflowVert :: c -> Overflow c
overflowVert = Overflow (Just Vertical)

overflow :: c -> Overflow c
overflow = Overflow Nothing

