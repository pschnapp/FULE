{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Grid
 ( GridM
 , Grid
 , grid
 ) where

import Data.Functor.Identity

import FULE.Component
import FULE.Container
import FULE.Container.Item
import FULE.Internal.Util
import FULE.Layout


-- | A two-dimensional grid of visual 'FULE.Container.Item.ItemM's.
data GridM m k
  = Grid
    { rowCountOf :: Int
    , columnCountOf :: Int
    , itemsOf :: [ItemM m k]
    }

-- | Like 'GridM' but run in the 'Data.Functor.Identity.Identity' monad.
type Grid = GridM Identity

instance (Monad m) => Container (GridM m k) k m where
  minWidth (Grid _ c is) p = fmap (* c) . getMaxSize <$> mapM (`minWidth` p) is
  minHeight (Grid r _ is) p = fmap (* r) . getMaxSize <$> mapM (`minHeight` p) is
  addToLayout (Grid r c is) proxy bounds renderGroup = do
    let addBetween f1 f2 p =
          addGuideToLayout $ Between (f1 bounds, p) (f2 bounds, 1-p)
    elasHorizs <- mapM (addBetween topOf bottomOf) (percents r)
    elasVerts <- mapM (addBetween leftOf rightOf) (percents c)
    let tops = topOf bounds : elasHorizs
    let lefts = leftOf bounds : elasVerts
    let rights = elasVerts ++ [rightOf bounds]
    let bottoms = elasHorizs ++ [bottomOf bounds]
    let boundsForItems =
          [Bounds t l r b
          | (t, b) <- zip tops bottoms
          , (l, r) <- zip lefts rights
          ]
    mapM_ (\(i, b) -> addToLayout i proxy b renderGroup) (zip is boundsForItems)

percents :: Int -> [Double]
percents n = fmap (\i -> fromIntegral i / fromIntegral n) [1..n-1]

-- | Create a 'GridM' of 'FULE.Container.Item.ItemM's.
grid
  :: Int -- ^ The number of rows the 'GridM' should have.
  -> Int -- ^ The number of columns the 'GridM' should have.
  -> [ItemM m k]
  -- ^ The 'FULE.Container.Item.ItemM's to put in the 'GridM'.
  --
  --   Placement of the 'FULE.Container.Item.ItemM's will start with the top,
  --   left position of the 'GridM' and proceed to the right, wrapping around
  --   to the next row when the end of the previous row has been reached.
  --
  --   If the number of elements in the list passed as an argument to this
  --   parameter does not meet or exceeds the number of 'GridM' locations
  --   available, then up-to the number of 'GridM' locations will be filled, but
  --   no more than that.
  -> GridM m k
grid rows cols = Grid (max 0 rows) (max 0 cols)

