{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  minWidth (Clipped c) proxy = minWidth c proxy
  minHeight (Clipped c) proxy = minHeight c proxy
  addToLayout (Clipped c) proxy bounds renderGroup =
    addToLayout c proxy bounds{ clippingOf = Just bounds } renderGroup

-- | Create a container which clips any overflow.
clipped :: c -> Clipped c
clipped = Clipped

