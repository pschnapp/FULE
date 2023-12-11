{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container.Padded
-- Description : The @Padded@ Container.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A 'FULE.Container.Container' to add padding around content.
module FULE.Container.Padded
 ( Padded
 , padded
 ) where

import FULE.Common
import FULE.Component
import FULE.Container
import FULE.Layout


-- | A container with padding around the content.
data Padded c
  = Padded
    { horizPaddingOf :: Int
    , vertPaddingOf :: Int
    , contentsOf :: c
    }

instance (Container c k m) => Container (Padded c) k m where
  minWidth (Padded h _ c) proxy = fmap (+ 2 * h) <$> minWidth c proxy
  minHeight (Padded _ v c) proxy = fmap (+ 2 * v) <$> minHeight c proxy
  addToLayout (Padded h v c) proxy bounds renderGroup = do
    let Bounds t l r b cl = bounds
    t' <- if v == 0 then return t else addGuideToLayout $ Relative v t Asymmetric
    l' <- if h == 0 then return l else addGuideToLayout $ Relative h l Asymmetric
    r' <- if h == 0 then return r else addGuideToLayout $ Relative (-h) r Asymmetric
    b' <- if v == 0 then return b else addGuideToLayout $ Relative (-v) b Asymmetric
    let bounds' = Bounds t' l' r' b' cl
    addToLayout c proxy bounds' renderGroup

-- | Create a container with padding around the content.
padded :: Padding -> c -> Padded c
padded (horiz, vert) = Padded (max 0 horiz) (max 0 vert)

