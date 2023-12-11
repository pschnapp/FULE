{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Sized
 ( Sized
 , sizedHoriz
 , sizedVert
 , sized
 ) where

import Control.Applicative

import FULE.Container


-- | A container which specifies or overrides the size of content in the layout.
--
--   To /remove/ the size of content from consideration during the layout process
--   see the 'FULE.Container.Unreckoned.Unreckoned' container.
data Sized c
  = Sized
    { widthOf :: Maybe Int
    , heightOf :: Maybe Int
    , contentsOf :: c
    }

instance (Container c k m) => Container (Sized c) k m where
  minWidth (Sized w _ c) p = (w <|>) <$> minWidth c p
  minHeight (Sized _ h c) p = (h <|>) <$> minHeight c p
  addToLayout (Sized _ _ c) = addToLayout c

-- | Add or override the horizontal size of the content.
sizedHoriz
  :: Int -- ^ The width the content should have.
  -> c -- ^ The content.
  -> Sized c
sizedHoriz width = Sized (Just $ max 0 width) Nothing

-- | Add or override the vertical size of the content.
sizedVert
  :: Int -- ^ The height the content should have.
  -> c -- ^ The content.
  -> Sized c
sizedVert height = Sized Nothing (Just $ max 0 height)

-- | Add or override the size of the content in both dimensions.
sized
  :: (Int, Int) -- ^ The width and height the content should have.
  -> c -- ^ The content.
  -> Sized c
sized (width, height) = Sized (Just $ max 0 width) (Just $ max 0 height)

