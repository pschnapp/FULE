module Container.Sized
 ( Sized
 , sizedHoriz
 , sizedVert
 , sized
 ) where

import Container


data Sized c
  = Sized
    { widthOf :: Maybe Int
    , heightOf :: Maybe Int
    , contentsOf :: c
    }

instance (Container c k) => Container (Sized c) k where
  requiredWidth sized _ = widthOf sized
  requiredHeight sized _ = heightOf sized
  addToLayout (Sized _ _ c) = addToLayout c

sizedHoriz :: Int -> c -> Sized c
sizedHoriz w = Sized (Just w) Nothing

sizedVert :: Int -> c -> Sized c
sizedVert h = Sized Nothing (Just h)

sized :: Int -> Int -> c -> Sized c
sized w h = Sized (Just w) (Just h)

