{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Sized
 ( Sized
 , sizedHoriz
 , sizedVert
 , sized
 ) where

import FULE.Container


-- | A container which specifies or overrides the size of content in the layout.
--
--   To /remove/ the size of content from consideration during layout process
--   see the 'FULE.Container.Unreckoned.Unreckoned' container.
data Sized c
  = Sized
    { widthOf :: Maybe Int
    , heightOf :: Maybe Int
    , contentsOf :: c
    }

instance (Container c k m) => Container (Sized c) k m where
  minWidth sized _ = return (widthOf sized)
  minHeight sized _ = return (heightOf sized)
  addToLayout (Sized _ _ c) = addToLayout c

-- | Add a horizontal size to the content.
sizedHoriz
  :: Int -- ^ The width the content should have.
  -> c -- ^ The content.
  -> Sized c
sizedHoriz width = Sized (Just $ max 0 width) Nothing

-- | Add a vertical size to the content.
sizedVert
  :: Int -- ^ The height the content should have.
  -> c -- ^ The content.
  -> Sized c
sizedVert height = Sized Nothing (Just $ max 0 height)

-- | Add a size to the content in both dimensions.
sized
  :: (Int, Int) -- ^ The width and height the content should have.
  -> c -- ^ The content.
  -> Sized c
sized (width, height) = Sized (Just $ max 0 width) (Just $ max 0 height)

