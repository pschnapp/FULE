{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container.Clipped
-- Description : The @Clipped@ Container.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A 'FULE.Container.Container' to specify that content overflow should be clipped.
module FULE.Container.Clipped
 ( Clipped
 , clipped
 ) where

import FULE.Component
import FULE.Container


-- | A container the content of which should be clipped on overflow.
--   Clipping bounds are specified as part of 'FULE.Component.Bounds' of
--   contained 'FULE.Component.Component's.
--
--   It is up to you the consumer to implement the actual clipping of content.
newtype Clipped c = Clipped c

instance (Container c k m) => Container (Clipped c) k m where
  minWidth (Clipped c) = minWidth c
  minHeight (Clipped c) = minHeight c
  addToLayout (Clipped c) proxy bounds =
    addToLayout c proxy bounds{ clippingOf = Just bounds }

-- | Create a container which clips any overflow.
clipped :: c -> Clipped c
clipped = Clipped

