{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container.Unreckoned
-- Description : The @Unreckoned@ Container.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A 'FULE.Container.Container' to remove dimension info from content when it's
-- considered during the layout process.
--
-- To /add/ size to content, see the 'FULE.Container.Sized.Sized' container.
module FULE.Container.Unreckoned
 ( Unreckoned
 , unreckonedHoriz
 , unreckonedVert
 , unreckoned
 ) where

import Control.Monad.Trans.Class
import Data.Maybe

import FULE.Component
import FULE.Container
import FULE.Container.Config
import FULE.Layout
import FULE.LayoutOp


-- | A container complementary to 'FULE.Container.Sized.Sized': whereas @Sized@
--   specifies a size for content which may or may not already have one,
--   @Unreckoned@ /removes/ size associated with content as it is reckoned
--   in the layout. This allows for content to overflow its bounds as far as the
--   layout is concerned.
--
--   The 'FULE.Component.Bounds' in the 'FULE.Component.ComponentInfo' for a
--   'FULE.Component.Component' which is @Unreckoned@ will match the overflowing
--   size of the @Component@ even though the size was not taken into account
--   during the layout process itself.
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
    let Bounds t l r b cl = bounds
    right <- addGuideToLayout $ Relative (fromMaybe 0 reqWidth) l Asymmetric
    bottom <- addGuideToLayout $ Relative (fromMaybe 0 reqHeight) t Asymmetric
    let bounds = Bounds t l right bottom cl
    addToLayout c proxy bounds renderGroup

-- | Elide the horizontal size of the content.
unreckonedHoriz :: c -> Unreckoned c
unreckonedHoriz = Unreckoned (Just Horizontal)

-- | Elide the verital size of the content.
unreckonedVert :: c -> Unreckoned c
unreckonedVert = Unreckoned (Just Vertical)

-- | Elide all sizes associated with the content.
unreckoned :: c -> Unreckoned c
unreckoned = Unreckoned Nothing

