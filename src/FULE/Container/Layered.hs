{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Layered
 ( LayeredM
 , Layered
 , layered
 ) where

import Data.Functor.Identity
import Data.Maybe

import FULE.Container
import FULE.Container.Item
import FULE.Internal.Util


-- | This container layers multiple visual 'FULE.Container.Item.ItemM's within
--   the same bounding rectangle, one on top of the other. Z-ordering is not
--   really taken into account since that depends on how you are using the
--   layout output, but the @ItemM@s will appear in the output in the same order
--   they're in when passed to the 'layered' function.
newtype LayeredM m k = Layered [ItemM m k]

-- | Like 'LayeredM' but run in the 'Data.Functor.Identity.Identity' monad.
type Layered = LayeredM Identity

instance (Monad m) => Container (LayeredM m k) k m where
  minWidth (Layered is) p = getMaxSize <$> mapM (`minWidth` p) is
  minHeight (Layered is) p = getMaxSize <$> mapM (`minHeight` p) is
  addToLayout (Layered is) proxy bounds renderGroup = do
    renderGroup' <- Just <$> maybe nextRenderGroup pure renderGroup
    mapM_ (\i -> addToLayout i proxy bounds renderGroup') is

-- | Layer 'FULE.Container.Item.ItemM's within the same bounding rectangle.
layered :: [ItemM m k] -> LayeredM m k
layered = Layered

