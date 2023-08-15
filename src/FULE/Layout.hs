module FULE.Layout
 ( LayoutDesign
 , makeDesign
 --
 , GuideID
 , DependencyType(..)
 , Positioning(..)
 , addGuide
 --
 , Constraint(..)
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

import FULE.Internal.Sparse as Matrix


data LayoutDesign
  = LayoutDesign
    { designPlasticityOf :: Matrix Double
    , designElasticityOf :: Matrix Double
    , designLTEConstraintsOf :: Matrix Double
    , designGTEConstraintsOf :: Matrix Double
    , designGuidesOf :: Matrix Double
    }

type LayoutDesignOp = LayoutDesign -> (GuideID, LayoutDesign)

makeDesign :: LayoutDesign
makeDesign = LayoutDesign empty empty empty empty empty


newtype GuideID = G Int
  deriving (Eq, Show)

data DependencyType = Asymmetric | Symmetric
  deriving (Eq, Show)

data Positioning
  = Absolute
    { positionOf :: Int
    }
  | Relative
    { offsetOf :: Int
    , dependencyOf :: GuideID
    , dependencyTypeOf :: DependencyType
    }
  | Between (GuideID, Double) (GuideID, Double)

addGuide :: Positioning -> LayoutDesignOp
addGuide (Absolute pos) = addAbsolute pos
addGuide (Relative offset gid dep) = addRelative offset gid dep 
addGuide (Between r1 r2) = addBetween r1 r2

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

addRelative :: Int -> GuideID -> DependencyType -> LayoutDesignOp
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


data Constraint = LTE | GTE
  deriving (Eq, Show)

addGuideConstraint :: GuideID -> Constraint -> GuideID -> LayoutDesign -> LayoutDesign
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

design :: Layout -> LayoutDesign
design layout =
  (layoutDesignOf layout) { designGuidesOf = layoutGuidesOf layout }

getGuide :: GuideID -> Layout -> Int
getGuide (G gid) = floor . get (gid, 1) . layoutGuidesOf

getGuides :: [GuideID] -> Layout -> [Int]
getGuides gs layout = map (`getGuide` layout) gs

reactToChange :: GuideID -> Int -> Layout -> Layout
reactToChange (G gid) amt =
  doReactToChanges [((gid, 1), fromIntegral amt)]

reactToChanges :: [(GuideID, Int)] -> Layout -> Layout
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

