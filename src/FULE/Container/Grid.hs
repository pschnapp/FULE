{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Grid
 ( Grid
 , grid
 ) where

import FULE.Container
import FULE.Container.Item
import FULE.Internal.Util
import FULE.Layout


data Grid k
  = Grid
    { rowCountOf :: Int
    , columnCountOf :: Int
    , itemsOf :: [Item k]
    }

instance Container (Grid k) k where
  requiredWidth (Grid _ c is) p =
    fmap (* c) . getMaxSize $ map (`requiredWidth` p) is
  requiredHeight (Grid r _ is) p =
    fmap (* r) . getMaxSize $ map (`requiredHeight` p) is
  addToLayout (Grid r c is) proxy bounds renderGroup = do
    let addBetween f1 f2 p =
          addGuideToLayout $ Between (f1 bounds, p) (f2 bounds, 1-p)
    elasHorizs <- mapM (addBetween topOf bottomOf) (percents r)
    elasVerts <- mapM (addBetween leftOf rightOf) (percents c)
    let addSymNeighbor g =
          addGuideToLayout $ Relative 1 g Symmetric
    relHorizs <- mapM addSymNeighbor elasHorizs
    relVerts <- mapM addSymNeighbor elasVerts
    let tops = topOf bounds : relHorizs
    let lefts = leftOf bounds : relVerts
    let rights = elasVerts ++ [rightOf bounds]
    let bottoms = elasHorizs ++ [bottomOf bounds]
    let boundsForItems =
          [Bounds t l r b
          | (t, b) <- zip tops bottoms
          , (l, r) <- zip lefts rights
          ]
    mapM_ (\(i, b) -> addToLayout i proxy b renderGroup) (zip is boundsForItems)

percents :: Int -> [Float]
percents n = fmap (\i -> fromIntegral i / fromIntegral n) [1..n-1]

grid :: Int -> Int -> [Item k] -> Grid k
grid rows cols = Grid (max 0 rows) (max 0 cols)

