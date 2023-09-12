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

import Control.DeepSeq

import FULE.Layout


-- | A typeclass for specifying the display requirements of a visual component.
--
--   You'll need to have the @MultiParamTypeClasses@ language extension
--   enabled to use this and you may wish to have your instance use the
--   @{-# OVERLAPS #-}@ or @{-# OVERLAPPING #-}@ pragmas.
class (Monad m) => Component k m where
  -- | The /width/ the component requires on-screen, if any.
  --   The default implementation returns @Nothing@.
  requiredWidth :: k -> m (Maybe Int)
  requiredWidth _ = return Nothing
  -- | The /height/ the compnent requires on-screen, if any.
  --   The default implementation returns @Nothing@.
  requiredHeight :: k -> m (Maybe Int)
  requiredHeight _ = return Nothing

-- | A convenience instance for components that have no width or height
--   requirements.
instance {-# OVERLAPPABLE #-} (Monad m) => Component k m where


-- | Meta info about a component along with the component itself.
data ComponentInfo k
  = ComponentInfo
    { boundsOf :: Bounds -- ^ The bounding rectangle of the component.
    , componentOf :: k -- ^ The component itself.
    , renderGroupOf :: RenderGroup -- ^ The render group of the component.
    }
  deriving (Functor, Show)

instance (NFData k) => NFData (ComponentInfo k) where
  rnf i@(ComponentInfo { boundsOf = b, componentOf = k, renderGroupOf = g }) =
    seq i . deepseq b . deepseq k . deepseq g $ ()


-- | A convenience type-wrapper representing the rendering group a component
--   is associated with. Rendering groups are used when multiple components
--   overlap (in the z-axis) or are part of containers which overlap.
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

instance NFData Bounds where
  rnf a@(Bounds t l r b) =
    seq a . deepseq t . deepseq l . deepseq r . deepseq b $ ()


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

