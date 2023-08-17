{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Unreckoned
 ( Unreckoned
 , unreckonedHoriz
 , unreckonedVert
 , unreckoned
 ) where

import Control.Monad.Trans.Class
import Data.Maybe

import FULE.Common
import FULE.Component
import FULE.Container
import FULE.Layout


-- | A container complementary to 'FULE.Container.Sized.Sized': whereas @Sized@
--   specifies a size for content which may or may not already have one,
--   @Unreckoned@ /removes/ the size associated with content as it is reckoned
--   in the layout. This allows for content to overflow its bounds as far as the
--   layout is concerned.
--
--   The 'FULE.Component.Bounds' of a 'FULE.Component.Component' which is
--   @Unreckoned@ will match the overflowing content size in the layout output.
data Unreckoned c = Unreckoned (Maybe Orientation) c

instance (Container c k m) => Container (Unreckoned c) k m where
  minWidth (Unreckoned o c) proxy = case o of
    Just Vertical -> minWidth c proxy
    _ -> return Nothing
  minHeight (Unreckoned o c) proxy = case o of
    Just Horizontal -> minHeight c proxy
    _ -> return Nothing
  addToLayout (Unreckoned _ c) proxy bounds renderGroup = do
    reqWidth <- lift . lift $ minWidth c proxy
    reqHeight <- lift . lift $ minHeight c proxy
    let Bounds t l r b = bounds
    right <- addGuideToLayout $ Relative (fromMaybe 0 reqWidth) l Asymmetric
    bottom <- addGuideToLayout $ Relative (fromMaybe 0 reqHeight) t Asymmetric
    let bounds = Bounds t l right bottom
    addToLayout c proxy bounds renderGroup

-- | Remove the horizontal size of content.
unreckonedHoriz :: c -> Unreckoned c
unreckonedHoriz = Unreckoned (Just Horizontal)

-- | Remove the verital size of the content.
unreckonedVert :: c -> Unreckoned c
unreckonedVert = Unreckoned (Just Vertical)

-- | Remove all sizes of the content.
unreckoned :: c -> Unreckoned c
unreckoned = Unreckoned Nothing

