{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : FULE.Reactor
-- Description : The @Reactor@ and @Reactor'@ types and the @Reaction@ typeclass.
-- Copyright   : (c) Paul Schnapp, 2024
-- License     : BSD3
-- Maintainer  : Paul Schnapp <paul.schnapp@gmail.com>
--
-- The terminology used in this module is based on that of a chemical reaction:
--
--  - A chemical reaction is given /reactants/ and produces /products/.
--  - Reactions are contained in /reactors/.
--
-- The @Reactor@ and (strict) @Reactor'@ types act as containers for
-- heterogeneous 'FULE.Component.Component' types.
--
-- The @Reaction@ typeclass establishes an interface for interacting with @Reactor@s'
-- contents (beyond what the @Component@ typeclass allows), providing for adding
-- in reactants (inputs) and getting out products (outputs).
module FULE.Reactor
 ( Reaction(..)
 , Reactor
 , reactor
 , Reactor'
 , reactor'
 ) where

import Control.DeepSeq

import FULE.Component


-- References:
-- https://wiki.haskell.org/Heterogenous_collections


-- | An interface for interacting with the contents of a @Reactor@:
--
--    - The @r@ type-parameter is the reacting type (inside the @Reactor@).
--    - The @i@ type-parameter is the type of input the reaction will accept
--      in the @addReactant@ method.
--    - The @ogadt@ type-parameter is a higher-order type used to specify what type
--      of output the @getProduct@ method should produce. You'll likely want to
--      use a GADT for this type.
--
--   __IMPORTANT NOTE:__ when implementing these methods you'll need a
--   catchall case for any input or output cases you're not handling.
class Reaction r i ogadt where

  -- | Add a reactant (input) to the reaction.
  addReactant :: i -> ogadt o -> r -> r
  addReactant _ _ = id

  -- | Get a product (output) from the reaction.
  getProduct :: i -> ogadt o -> r -> Maybe o
  getProduct _ _ _ = Nothing


-- | A container for heterogeneous types. The contents can be interacted with
--   using the 'Reaction' typeclass.
data Reactor i ogadt m =
  forall r . (Component r m, Reaction r i ogadt) => Reactor r

instance (Monad m) => Component (Reactor i ogadt m) m where
  requiredWidth (Reactor r) = requiredWidth r
  requiredHeight (Reactor r) = requiredHeight r

instance Reaction (Reactor i ogadt m) i ogadt where
  addReactant i ogadt (Reactor r) = Reactor (addReactant i ogadt r)
  getProduct i ogadt (Reactor r) = getProduct i ogadt r

-- | Construct a @Reactor@.
reactor :: (Component r m, Reaction r i ogadt) => r -> Reactor i ogadt m
reactor = Reactor


-- | A container for heterogeneous types which is strict in its contents.
--   The contents can be interacted with using the 'Reaction' typeclass.
--
--   In addition to implementing the @Component@ and @Reaction@ typeclasses like
--   the 'Reactor' type, this type implements the 'Control.DeepSeq.NFData'
--   typeclass from @Control.DeepSeq@ so evaluation of its contents can be forced.
data Reactor' i ogadt m =
  forall r . (Component r m, NFData r, Reaction r i ogadt) => Reactor' !r

instance (Monad m) => Component (Reactor' i ogadt m) m where
  requiredWidth (Reactor' r) = requiredWidth r
  requiredHeight (Reactor' r) = requiredHeight r

instance NFData (Reactor' i ogadt m) where
  rnf (Reactor' r) = rnf r

instance Reaction (Reactor' i ogadt m) i ogadt where
  addReactant i ogadt (Reactor' r) = Reactor' (addReactant i ogadt r)
  getProduct i ogadt (Reactor' r) = getProduct i ogadt r

-- | Construct a @Reactor'@.
reactor' :: (Component r m, NFData r, Reaction r i ogadt) => r -> Reactor' i ogadt m
reactor' = Reactor'

