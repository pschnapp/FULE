module Container.Grid
 ( Grid
 , grid
 ) where

import Control.Monad

import Container
import Container.Item
import Internal.Util


data Grid k
  = Grid
    { rowsOf :: Int
    , columnsOf :: Int
    , itemsOf :: [Item k]
    }

instance Container (Grid k) k where
  requiredWidth (Grid _ c is) p =
    fmap (* c) . getMaxSize $ map (`requiredWidth` p) is
  requiredHeight (Grid r _ is) p =
    fmap (* r) . getMaxSize $ map (`requiredHeight` p) is
  addToLayout (Grid r c is) bounds renderGroup = do
    -- TODO:
    -- make guides for each grid item and combine them into bounds
    -- (make elastics and symmetric one over/down for neighboring positions)
    -- zip bounds with `is` and run through mapM_ below
    mapM_ (\i -> addToLayout i bounds renderGroup) is

grid :: Int -> Int -> [Item k] -> Grid k
grid = Grid

