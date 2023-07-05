{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FULE.Component
 ( Component(..)
 , ComponentInfo(..)
 , RenderGroup
 , Bounds(..)
 , HasBoundingGuides(..)
 , boundingGuidesInCSSOrderFor
 ) where

import FULE.Layout


class (Monad m) => Component k m where
  requiredWidth :: k -> m (Maybe Int)
  requiredWidth _ = return Nothing

  requiredHeight :: k -> m (Maybe Int)
  requiredHeight _ = return Nothing

instance {-# OVERLAPPABLE #-} (Monad m) => Component k m where


data ComponentInfo k
  = ComponentInfo
    { boundsOf :: Bounds
    , componentOf :: k
    , renderGroupOf :: RenderGroup
    }
  deriving (Functor, Show)


type RenderGroup = Maybe Int


data Bounds
  = Bounds
    { topOf :: GuideID
    , leftOf :: GuideID
    , rightOf :: GuideID
    , bottomOf :: GuideID
    }
  deriving (Show)


class HasBoundingGuides a where
  boundingGuidesFor :: Layout -> a -> [Int]

instance HasBoundingGuides Bounds where
  boundingGuidesFor layout (Bounds t l r b) =
    getGuides [t, l, r, b] layout

instance HasBoundingGuides (ComponentInfo k) where
  boundingGuidesFor layout component =
    boundingGuidesFor layout (boundsOf component)

boundingGuidesInCSSOrderFor :: (HasBoundingGuides b) => Layout -> b -> [Int]
boundingGuidesInCSSOrderFor layout component =
  let [t, l, r, b] = boundingGuidesFor layout component
  in [t, r, b, l]

