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


newtype LayeredM m k = Layered [ItemM m k]

type Layered = LayeredM Identity

-- TODO
--  - render order
--  - input censoring
instance (Monad m) => Container (LayeredM m k) k m where
  minWidth (Layered is) p = getMaxSize <$> mapM (`minWidth` p) is
  minHeight (Layered is) p = getMaxSize <$> mapM (`minHeight` p) is
  addToLayout (Layered is) proxy bounds renderGroup = do
    renderGroup' <- Just <$> maybe nextRenderGroup pure renderGroup
    mapM_ (\i -> addToLayout i proxy bounds renderGroup') is

layered :: [ItemM m k] -> LayeredM m k
layered = Layered

