{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Layered
 ( Layered
 , layered
 ) where

import Data.Maybe

import FULE.Container
import FULE.Container.Item
import FULE.Internal.Util


newtype Layered k = Layered [Item k]

-- TODO
--  - render order
--  - input censoring
instance Container (Layered k) k where
  requiredWidth (Layered is) p = getMaxSize $ map (`requiredWidth` p) is
  requiredHeight (Layered is) p = getMaxSize $ map (`requiredHeight` p) is
  addToLayout (Layered is) proxy bounds renderGroup = do
    renderGroup' <- Just <$> maybe nextRenderGroup pure renderGroup
    mapM_ (\i -> addToLayout i proxy bounds renderGroup') is

layered :: [Item k] -> Layered k
layered = Layered

