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


-- | A container for heterogenous items. This container lets items of different
--   types be used in the same aggregating container, like
--   'FULE.Container.Arrayed.ArrayedM', 'FULE.Container.Grid.GridM', or
--   'FULE.Container.Layered.LayeredM'.
--
--   When using @ItemM@ you'll likely need to:
--   
--   * Use the [@ScopedTypeVariables@](https://wiki.haskell.org/Scoped_type_variables)
--     language extension and explicitly specify a @forall@ for @m@ in your
--     function declaration (if a type-variable is being used)
--   * Explicitly specify the type of your list of @ItemM@ in the call to the
--     aggregating container and wrap the list in parentheses
--
--   For example:
--
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- >
-- > import FULE
-- >
-- > ...
-- >
-- > someFn :: forall m => m (ArrayedM m Widget)
-- > someFn = return $
-- >   arrayedHoriz noPadding
-- >     ([item someWidget
-- >     , item someContainer
-- >     , item someOtherWidget
-- >     ]::[ItemM m Widget])
--
--   [Reference: Heterogenous Collections](https://wiki.haskell.org/Heterogenous_collections)
data ItemM m k = forall c . Container c k m => Item c

-- | Like 'ItemM' but run in the 'Data.Functor.Identity.Identity' monad.
type Item = ItemM Identity

instance (Monad m) => Container (ItemM m k) k m where
  minWidth (Item c) = minWidth c
  minHeight (Item c) = minHeight c
  addToLayout (Item c) = addToLayout c

-- | Create an 'ItemM' with a heterogenous item.
item :: (Container c k m) => c -> ItemM m k
item = Item

