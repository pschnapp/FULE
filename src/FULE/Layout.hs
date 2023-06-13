module FULE.Layout
 ( GuideID
 , DependencyType(..)
 , Relationship(..)
 , LayoutDesign
 , makeDesign
 , addGuide
 , Layout
 , build
 , design
 , getGuide
 , getGuides
 , reactToChange
 , reactToChanges
 ) where

import FULE.Internal.Sparse


newtype GuideID = G Int
  deriving (Show)


data DependencyType = Asymmetric | Symmetric
  deriving (Show)


data Relationship
  = Absolute
    { positionOf :: Int
    }
  | Relative
    { offsetOf :: Int
    , dependencyOf :: GuideID
    , dependencyTypeOf :: DependencyType
    }
  | Between (GuideID, Double) (GuideID, Double)


data LayoutDesign
  = LayoutDesign
    { designPlasticityOf :: Matrix Double
    , designElasticityOf :: Matrix Double
    , designConstraintsOf :: Matrix Double
    , designGuidesOf :: Matrix Double
    }

type LayoutDesignOp = LayoutDesign -> (GuideID, LayoutDesign)

makeDesign :: LayoutDesign
makeDesign = LayoutDesign empty empty empty empty

addGuide :: Relationship -> LayoutDesignOp
addGuide (Absolute pos) = addAbsolute pos
addGuide (Relative offset gid dep) = addRelative offset gid dep 
addGuide (Between r1 r2) = addBetween r1 r2

addAbsolute :: Int -> LayoutDesignOp
addAbsolute position design =
  ( G gid
  , LayoutDesign
    { designPlasticityOf = set (gid, gid) 1 (designPlasticityOf design)
    , designElasticityOf = set (gid, gid) 1 (designElasticityOf design)
    , designConstraintsOf = expandTo (gid, gid) (designConstraintsOf design)
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
    , designConstraintsOf = expandTo (gid, gid) (designConstraintsOf design)
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
    , designConstraintsOf = expandTo (gid, gid) (designConstraintsOf design)
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


data Layout
  = Layout
    { layoutPlasticityOf :: Matrix Double
    , layoutElasticityOf :: Matrix Double
    , layoutConstraintsOf :: Matrix Double
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
  { layoutPlasticityOf = designPlasticityOf design
  , layoutElasticityOf = designElasticityOf design
  , layoutConstraintsOf = designConstraintsOf design
  , layoutTransformationOf =
      propagate (designPlasticityOf design)
      `mul` designElasticityOf design
  , layoutGuidesOf = designGuidesOf design
  }

design :: Layout -> LayoutDesign
design layout =
  LayoutDesign
  { designPlasticityOf = layoutPlasticityOf layout
  , designElasticityOf = layoutElasticityOf layout
  , designConstraintsOf = layoutConstraintsOf layout
  , designGuidesOf = layoutGuidesOf layout
  }

getGuide :: GuideID -> Layout -> Int
getGuide (G gid) = floor . get (gid, 1) . layoutGuidesOf

getGuides :: [GuideID] -> Layout -> [Int]
getGuides gs layout = map (`getGuide` layout) gs

reactToChange :: GuideID -> Int -> Layout -> Layout
reactToChange (G gid) amt l@(Layout { layoutTransformationOf = t, layoutGuidesOf = g }) =
  l { layoutGuidesOf = add g (t `mul` changes) }
  where
    changes = matrix (dims g) [((gid, 1), fromIntegral amt)]

reactToChanges :: [(GuideID, Int)] -> Layout -> Layout
reactToChanges pairs l@(Layout { layoutTransformationOf = t, layoutGuidesOf = g }) =
  l { layoutGuidesOf = add g (t `mul` changes) }
  where
    convert (G gid, amt) = ((gid, 1), fromIntegral amt)
    changes = matrix (dims g) (map convert pairs)



-- constraints:
-- x <= y
-- x >= y
--
-- if x <= y then y-x will be >= 0
-- if x >= y then x-y will be >= 0
--
-- don't think this can be done w/ a matrix, since each pair has to be done independently
-- unless there's just a single constraint max for each guide
-- and adjustments are not cumulative, but the max (negative) number

