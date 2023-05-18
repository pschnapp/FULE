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
  | Between (GuideID, Float) (GuideID, Float)


data LayoutDesign
  --    dependency     elasticity     constraint   guides (vector)
  = D (Matrix Float) (Matrix Float) (Matrix Float) (Matrix Float)

type LayoutDesignOp = LayoutDesign -> (GuideID, LayoutDesign)

makeDesign :: LayoutDesign
makeDesign = D empty empty empty empty

addGuide :: Relationship -> LayoutDesignOp
addGuide (Absolute pos) = addAbsolute pos
addGuide (Relative offset gid dep) = addRelative offset gid dep 
addGuide (Between r1 r2) = addBetween r1 r2

addAbsolute :: Int -> LayoutDesignOp
addAbsolute position (D deps elas cons guides) =
  (G gid, D deps' elas' cons' guides')
  where
    (r, _) = dims deps
    gid = r + 1  
    deps' = set (gid, gid) 1 deps
    elas' = set (gid, gid) 1 elas
    cons' = expandTo (dims elas') cons
    guides' = set (gid, 1) (fromIntegral position) guides

addRelative :: Int -> GuideID -> DependencyType -> LayoutDesignOp
addRelative offset (G ref) dep (D deps elas cons guides) =
  (G gid, D deps' elas' cons' guides')
  where
    (r, _) = dims deps
    gid = r + 1
    deps' = set (gid, gid) 1 . set (gid, ref) 1 $
      case dep of
        Asymmetric -> deps
        Symmetric  -> set (ref, gid) 1 deps
    elas' = set (gid, gid) 1 elas
    cons' = expandTo (dims elas') cons
    pos = fromIntegral offset + get (ref, 1) guides
    guides' = set (gid, 1) pos guides

addBetween :: (GuideID, Float) -> (GuideID, Float) -> LayoutDesignOp
addBetween (G ref1, pct1) (G ref2, pct2) (D deps elas cons guides) =
  (G gid, D deps' elas' cons' guides')
  where
    (r, _) = dims deps
    gid = r + 1
    deps' = set (gid, gid) 1 deps
    -- yes the percents are supposed to be flipped in these:
    elas' = set (gid, gid) 1 . set (gid, ref1) pct2 . set (gid, ref2) pct1 $ elas
    cons' = expandTo (dims elas') cons
    pos = pct2 * get (ref1, 1) guides + pct1 * get (ref2, 1) guides
    guides' = set (gid, 1) pos guides


data Layout
  = L
    { dependencyMatrixOf :: Matrix Float
    , elasticityMatrixOf :: Matrix Float
    , constraintMatrixOf :: Matrix Float
    , propagatedMatrixOf :: Matrix Float
    , guidesVectorOf :: Matrix Float
    }

instance Show Layout where
  show l = concat ["\n", show (propagatedMatrixOf l), "\n\n", show (guidesVectorOf l), "\n"]

propagate :: (Num a) => Matrix a -> Matrix a
propagate m =
  let m' = fmap (const 1) $ m `mul` m
  in if count m' == count m
  then m'
  else propagate m'

build :: LayoutDesign -> Layout
build (D deps elas cons guides) = L deps elas cons prop guides
  where prop = propagate deps `mul` elas

design :: Layout -> LayoutDesign
design (L d e c _ g) = D d e c g

getGuide :: GuideID -> Layout -> Int
getGuide (G gid) = floor . get (gid, 1) . guidesVectorOf

getGuides :: [GuideID] -> Layout -> [Int]
getGuides gs layout = map (`getGuide` layout) gs

reactToChange :: GuideID -> Int -> Layout -> Layout
reactToChange (G gid) amt l@(L _ _ _ p g) = l { guidesVectorOf = add g (p `mul` changes) }
  where
    changes = matrix (dims g) [((gid, 1), fromIntegral amt)]

reactToChanges :: [(GuideID, Int)] -> Layout -> Layout
reactToChanges pairs l@(L _ _ _ p g) = l { guidesVectorOf = add g (p `mul` changes) }
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

