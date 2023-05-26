{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Container.Item
 ( ItemM
 , Item
 , item
 ) where

import Data.Functor.Identity

import FULE.Container


-- a type for anything containing heterogenous items
-- https://wiki.haskell.org/Heterogenous_collections
data ItemM m k = forall c . Container c k m => Item c

type Item = ItemM Identity

instance (Monad m) => Container (ItemM m k) k m where
  minWidth (Item c) = minWidth c
  minHeight (Item c) = minHeight c
  addToLayout (Item c) = addToLayout c

item :: (Container c k m) => c -> ItemM m k
item = Item

