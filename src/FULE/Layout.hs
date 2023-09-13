module FULE.Layout
 ( LayoutDesign
 , makeLayoutDesign
 --
 , GuideID
 , PlasticDependencyType(..)
 , GuideSpecification(..)
 , addGuide
 --
 , GuideConstraintType(..)
 , addGuideConstraint
 --
 , Layout
 , build
 , design
 , getGuide
 , getGuides
 , reactToChange
 , reactToChanges
 ) where

import Control.DeepSeq

import FULE.Internal.Sparse as Matrix


--------------------------------
-- LayoutDesign
--------------------------------

-- | A 'Layout' that is still under construction.
--   Use the 'build' function to turn a @LayoutDesign@ into an elivened @Layout@.
data LayoutDesign
  = LayoutDesign
    { designPlasticityOf :: Matrix Double
    , designElasticityOf :: Matrix Double
    , designLTEConstraintsOf :: Matrix Double
    , designGTEConstraintsOf :: Matrix Double
    , designGuidesOf :: Matrix Double
    }

-- | Create a new 'LayoutDesign'.
makeLayoutDesign :: LayoutDesign
makeLayoutDesign = LayoutDesign empty empty empty empty empty


-- | An identifier for a Guide in a 'Layout' or 'LayoutDesign'.
newtype GuideID = G Int
  deriving (Eq, Ord, Read, Show)

instance NFData GuideID where
  rnf g@(G i) = seq g . deepseq i $ ()


-- | The type of a plastic dependency between two Guides.
data PlasticDependencyType
  = Asymmetric
  -- ^ Specifies that changes to the dependent Guide do not affect the reference
  --   Guide, but changes to the reference propagate to the dependent Guide.
  | Symmetric
  -- ^ Specifies that changes to either Guide are applied to the other as well.
  deriving (Eq, Show)

-- | The specification of a Guide to be added to a 'LayoutDesign'.
--   A Guide may be added:
--
--   * at an absolute position within the design
--   * relative to a reference Guide within the design with a plastic
--     dependencey upon the reference
--   * relative to two reference Guides within the design with an elastic
--     dependency upon both
--
--   See each constructor and its fields for more information.
data GuideSpecification
  = Absolute -- ^ Add a new Guide at an absolute position within the @Layout@.
    { positionOf :: Int
    -- ^ The position the new Guide should have in the @Layout@.
    --   Note this could be either an @x@ or @y@ position, the axis doesn't
    --   matter for the specification.
    }
  | Relative -- ^ Add a new Guide with a plastic dependence on a reference Guide.
    { offsetOf :: Int
    -- ^ The offset from the reference Guide the new dependent Guide should have.
    , dependencyOf :: GuideID
    -- ^ The ID of the reference Guide.
    , dependencyTypeOf :: PlasticDependencyType
    -- ^ The type of dependency the dependent Guide should have on the reference
    --   Guide.
    }
  | Between
    -- ^ Add a new Guide between two other Guides with an elastic dependency on them:
    --   Whenever one of the reference Guides moves the dependent Guide will be moved
    --   to remain positioned relatively between them.
    --
    --   The @Double@ arguments of the pairs below should sum to equal @1.0@;
    --   this will not be checked.
      (GuideID, Double)
      -- ^ A reference Guide and how close the dependent Guide should be to it
      --   relative to the other reference, as a percentage.
      (GuideID, Double)
      -- ^ Another reference Guide and how close the dependent Guide should be
      --   to it relative to the first reference, as a percentage.


-- | Add a new Guide to a 'LayoutDesign' according to the given 'GuideSpecification'.
--
--   Returns an ID for the new Guide along with an updated 'LayoutDesign'.
addGuide :: GuideSpecification -> LayoutDesign -> (GuideID, LayoutDesign)
addGuide (Absolute pos) = addAbsolute pos
addGuide (Relative offset gid dep) = addRelative offset gid dep 
addGuide (Between r1 r2) = addBetween r1 r2

type LayoutDesignOp = LayoutDesign -> (GuideID, LayoutDesign)

addAbsolute :: Int -> LayoutDesignOp
addAbsolute position design =
  ( G gid
  , LayoutDesign
    { designPlasticityOf = set (gid, gid) 1 (designPlasticityOf design)
    , designElasticityOf = set (gid, gid) 1 (designElasticityOf design)
    , designLTEConstraintsOf = expandTo (gid, gid) (designLTEConstraintsOf design)
    , designGTEConstraintsOf = expandTo (gid, gid) (designGTEConstraintsOf design)
    , designGuidesOf = set (gid, 1) (fromIntegral position) (designGuidesOf design)
    }
  )
  where
    gid = nextGuideNumberFor design

addRelative :: Int -> GuideID -> PlasticDependencyType -> LayoutDesignOp
addRelative offset (G ref) dep design@(LayoutDesign { designGuidesOf = guides }) =
  ( G gid
  , LayoutDesign
    { designPlasticityOf =
        set (gid, gid) 1 . set (gid, ref) 1 . symRelat $ designPlasticityOf design
    , designElasticityOf = set (gid, gid) 1 (designElasticityOf design)
    , designLTEConstraintsOf = expandTo (gid, gid) (designLTEConstraintsOf design)
    , designGTEConstraintsOf = expandTo (gid, gid) (designGTEConstraintsOf design)
    , designGuidesOf = set (gid, 1) pos guides
    }
  )
  where
    gid = nextGuideNumberFor design
    symRelat = case dep of
      Asymmetric -> id
      Symmetric  -> set (ref, gid) 1
    pos = fromIntegral offset + get (ref, 1) guides

addBetween :: (GuideID, Double) -> (GuideID, Double) -> LayoutDesignOp
addBetween (G ref1, pct1) (G ref2, pct2) design@(LayoutDesign { designGuidesOf = guides }) =
  ( G gid
  , LayoutDesign
    { designPlasticityOf = set (gid, gid) 1 (designPlasticityOf design)
    , designElasticityOf =
        -- yes the indicies are supposed to mismatch in this
        set (gid, gid) 1 . set (gid, ref1) pct2 . set (gid, ref2) pct1 $
          designElasticityOf design
    , designLTEConstraintsOf = expandTo (gid, gid) (designLTEConstraintsOf design)
    , designGTEConstraintsOf = expandTo (gid, gid) (designGTEConstraintsOf design)
    , designGuidesOf = set (gid, 1) pos guides
    }
  )
  where
    gid = nextGuideNumberFor design
    -- yes the indicies are supposed to mismatch in this
    pos = pct2 * get (ref1, 1) guides + pct1 * get (ref2, 1) guides

nextGuideNumberFor :: LayoutDesign -> Int
nextGuideNumberFor (LayoutDesign { designGuidesOf = guides }) =
  (+1) . fst $ dims guides


--------------------------------
-- Guide Constraints
--------------------------------

-- | The type of constraint one Guide should have relative to another.
data GuideConstraintType
  = LTE -- ^ Constrain a Guide to always be less-than or equal-to another.
  | GTE -- ^ Constrain a Guide to always be greater-than or equal-to another.
  deriving (Eq, Show)

-- | Constrain the movement of one Guide relative to another.
--
--   __Important Notes:__
--
--   * __This feature is experimental!__
--   * A Guide should be used /only once/ as the constrainee (first argument)
--     for a given constraint-type -- this will not be checked!
addGuideConstraint
  :: GuideID -- ^ The Guide to constrain the movement of.
  -> GuideConstraintType -- ^ The constraint-type.
  -> GuideID -- ^ The reference Guide to constrain movement relative to.
  -> LayoutDesign -> LayoutDesign
addGuideConstraint (G forGuide) constraint (G ofGuide) design =
  case constraint of
    LTE ->
      design
      { designLTEConstraintsOf =
          set (forGuide, forGuide) 1
          . set (forGuide, ofGuide) (-1)
          $ designLTEConstraintsOf design
      }
    GTE ->
      design
      { designGTEConstraintsOf =
          set (forGuide, forGuide) 1
          . set (forGuide, ofGuide) (-1)
          $ designGTEConstraintsOf design
      }


--------------------------------
-- Layout
--------------------------------

-- | A 'LayoutDesign' that has been enlivened and can have its Guides queried or
--   moved.
data Layout
  = Layout
    { layoutDesignOf :: LayoutDesign
    , layoutLTEConstraintsOf :: Matrix Double
    , layoutGTEConstraintsOf :: Matrix Double
    , layoutTransformationOf :: Matrix Double
    , layoutGuidesOf :: Matrix Double
    }

instance Show Layout where
  show l = concat
    [ "\n"
    , show (layoutTransformationOf l)
    , "\n\n"
    , show (layoutGuidesOf l)
    , "\n"
    ]

propagate :: (Num a) => Matrix a -> Matrix a
propagate m =
  let m' = fmap (const 1) $ m `mul` m
  in if count m' == count m
  then m'
  else propagate m'

-- | Create an enlivened 'Layout' from a 'LayoutDesign'.
build :: LayoutDesign -> Layout
build design =
  Layout
  { layoutDesignOf = design
  , layoutLTEConstraintsOf = propagated `mul` designLTEConstraintsOf design
  , layoutGTEConstraintsOf = propagated `mul` designGTEConstraintsOf design
  , layoutTransformationOf = propagated `mul` designElasticityOf design
  , layoutGuidesOf = designGuidesOf design
  }
  where
    propagated = propagate (designPlasticityOf design)

-- | Transform a 'Layout' back into a 'LayoutDesign'.
design :: Layout -> LayoutDesign
design layout =
  (layoutDesignOf layout) { designGuidesOf = layoutGuidesOf layout }

-- | Get the position of a Guide within a 'Layout'.
getGuide :: GuideID -> Layout -> Int
getGuide (G gid) = floor . get (gid, 1) . layoutGuidesOf

-- | Get the position of multiple Guides within a 'Layout'.
getGuides :: [GuideID] -> Layout -> [Int]
getGuides gs layout = map (`getGuide` layout) gs

-- | Move a Guide within a 'Layout'.
reactToChange
  :: GuideID -- ^ The Guide to move.
  -> Int -- ^ The movement to apply to the Guide -- a delta.
  -> Layout -> Layout
reactToChange (G gid) amt =
  doReactToChanges [((gid, 1), fromIntegral amt)]

-- | Move multiple Guides within a 'Layout'.
reactToChanges
  :: [(GuideID, Int)]
  -- ^ A list of Guides with movements (deltas) to apply to them.
  -> Layout -> Layout
reactToChanges pairs =
  let convert (G gid, amt) = ((gid, 1), fromIntegral amt)
  in doReactToChanges (map convert pairs)

doReactToChanges :: [(Pos, Double)] -> Layout -> Layout
doReactToChanges entries layout =
  layout { layoutGuidesOf = adjusted }
  where
    Layout
      { layoutLTEConstraintsOf = lte
      , layoutGTEConstraintsOf = gte
      , layoutTransformationOf = t
      , layoutGuidesOf = g
      } = layout
    changes = matrix (dims g) entries
    changed = t `mul` changes `add` g
    adjusted = changed
      `sub` Matrix.filter (> 0) (lte `mul` changed)
      `sub` Matrix.filter (< 0) (gte `mul` changed)

