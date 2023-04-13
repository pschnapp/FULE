module Container.Layered
 ( Layered
 , layered
 ) where

import Data.Maybe

import Container
import Container.Item
import Internal.Util


data Layered k = Layered [Item k]

-- TODO
--  - render order
--  - input censoring
instance Container (Layered k) k where
  requiredWidth (Layered is) p = getMaxSize $ map (`requiredWidth` p) is
  requiredHeight (Layered is) p = getMaxSize $ map (`requiredHeight` p) is
  addToLayout (Layered is) bounds renderGroup = do
    renderGroup' <- fmap Just $ fromMaybe nextRenderGroup $ pure <$> renderGroup
    mapM_ (\i -> addToLayout i bounds renderGroup') is

layered :: [Item k] -> Layered k
layered = Layered

