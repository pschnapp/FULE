{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Container
-- Description : The @Container@ typeclass.
-- Copyright   : (c) Paul Schnapp, 2023
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- A typeclass for creating containers of visual content.
module FULE.Container
 ( Container(..)
 ) where

import Control.Monad.Writer
import Data.Proxy

import FULE.Component
import FULE.LayoutOp


-- sadly the `Proxy` has to be used for the heterogenous collections to work
--
-- | A typeclass for laying-out a container of type @c@ that holds content of
--   type @k@.
--
--   You'll need to have the @FlexibleInstances@ and @MultiParamTypeClasses@
--   language extensions enabled to use this and you may wish to have your
--   instance use the @{-# OVERLAPS #-}@ or @{-# OVERLAPPING #-}@ pragmas.
class (Monad m) => Container c k m where
  -- | Get the minimum /width/ required for display of the container @c@.
  minWidth :: c -> Proxy k -> m (Maybe Int)
  -- | Get the minimum /height/ required for display of the container @c@.
  minHeight :: c -> Proxy k -> m (Maybe Int)
  -- | Add the container @c@ and its contents @k@ to a 'FULE.Layout.LayoutDesign'.
  addToLayout :: c -> Proxy k -> Bounds -> RenderGroup -> LayoutOp k m ()

instance {-# OVERLAPPABLE #-} (Component k m) => Container k k m where
  minWidth k _ = requiredWidth k
  minHeight k _ = requiredHeight k
  addToLayout k _ bounds renderGroup = tell [ComponentInfo bounds k renderGroup]

