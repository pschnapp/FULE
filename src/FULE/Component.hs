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


-- | A typeclass for specifying the display requirements of a visual component.
--
--   You may wish to have your instance use the @{-# OVERLAPS #-}@ or
--   @{-# OVERLAPPING #-}@ pragmas.
class (Monad m) => Component k m where
  -- | The /width/ the component requires on-screen, if any;
  --   the default implementation returns @Nothing@.
  requiredWidth :: k -> m (Maybe Int)
  requiredWidth _ = return Nothing
  -- | The /height/ the compnent requires on-screen, if any;
  --   the default implementation returns @Nothing@.
  requiredHeight :: k -> m (Maybe Int)
  requiredHeight _ = return Nothing

-- | A convenience instance for components that have no width or height
--   requirements.
instance {-# OVERLAPPABLE #-} (Monad m) => Component k m where


-- | Meta info about a component.
data ComponentInfo k
  = ComponentInfo
    { boundsOf :: Bounds -- ^ The bounding rectangle of the component.
    , componentOf :: k -- ^ The component itself.
    , renderGroupOf :: RenderGroup -- ^ The render group of the component.
    }
  deriving (Functor, Show)


-- | A convenience type-wrapper representing the rendering group a component
--   is associated with. Rendering groups spcify what needs to be redrawn when
--   multiple components overlap.
type RenderGroup = Maybe Int


-- | A collection of Guides representing the bounding rectangle of a visual
--   component.
data Bounds
  = Bounds
    { topOf :: GuideID
    -- ^ The Guide representing the /top/ edge of the bounding rectangle.
    , leftOf :: GuideID
    -- ^ The Guide representing the /left/ edge of the bounding rectangle.
    , rightOf :: GuideID
    -- ^ The Guide representing the /right/ edge of the bounding rectangle.
    , bottomOf :: GuideID
    -- ^ The Guide representing the /bottom/ edge of the bounding rectangle.
    }
  deriving (Show)


-- | A typeclass for retrieving Guides representing a bounding rectangle.
class HasBoundingGuides a where
  -- | Retrieves the bounding Guides for a type in the order:
  --   /top/, /left/, /right/, /bottom/.
  boundingGuidesFor :: Layout -> a -> [Int]

instance HasBoundingGuides Bounds where
  boundingGuidesFor layout (Bounds t l r b) =
    getGuides [t, l, r, b] layout

instance HasBoundingGuides (ComponentInfo k) where
  boundingGuidesFor layout component =
    boundingGuidesFor layout (boundsOf component)

-- | Retrieves the bounding Guides for a type in CSS order:
--   /top/, /right/, /bottom/, /left/.
boundingGuidesInCSSOrderFor :: (HasBoundingGuides a) => Layout -> a -> [Int]
boundingGuidesInCSSOrderFor layout component =
  let [t, l, r, b] = boundingGuidesFor layout component
  in [t, r, b, l]

