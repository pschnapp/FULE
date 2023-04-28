{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
sizedHoriz width = Sized (Just $ max 0 width) Nothing

sizedVert :: Int -> c -> Sized c
sizedVert height = Sized Nothing (Just $ max 0 height)

sized :: Int -> Int -> c -> Sized c
sized width height = Sized (Just $ max 0 width) (Just $ max 0 height)

