{-# LANGUAGE ExistentialQuantification #-}

module Container.Item
 ( Item
 , item
 ) where

import Container


-- a type for anything containing heterogenous items
-- https://wiki.haskell.org/Heterogenous_collections
data Item k = forall c . Container c k => Item c

instance Container (Item k) k where
  requiredWidth (Item c) = requiredWidth c
  requiredHeight (Item c) = requiredHeight c
  addToLayout (Item c) = addToLayout c

item :: (Container c k) => c -> Item k
item = Item

