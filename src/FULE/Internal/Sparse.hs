module FULE.Internal.Sparse
 ( Matrix
 , Pos
 , empty
 , matrix
 , dims
 , expandTo
 , get
 , set
 , del
 , count
 , add
 , sub
 , mul
 , filter
 ) where

import Prelude hiding (filter)
import Data.List hiding (filter)
import qualified Data.Map as Map


type Pos = (Int, Int)

maxp (rl, cl) (rr, cr) = (max rl rr, max cl cr)


data Matrix a
  = M
    { dimensionsOf :: (Int, Int)
    , matrixOf :: Map.Map Pos a
    }

instance (Num a, Show a) => Show (Matrix a) where
  show (M (0, 0) _) = "[]"
  show m@(M (r, c) _) = concat ["[ ", mat, "\n]"]
    where
      row rx = intercalate ", " $ map (\cx -> show $ get (rx, cx) m) [1..c]
      mat = intercalate "\n; " $ map row [1..r]

instance Functor Matrix where
  fmap f (M d m) = M d $ Map.map f m

empty :: Matrix a
empty = M (0, 0) Map.empty

matrix :: (Int, Int) -> [(Pos, a)] -> Matrix a
matrix dims entries = M dims (Map.fromList entries)

dims :: Matrix a -> (Int, Int)
dims = dimensionsOf

expandTo :: (Int, Int) -> Matrix a -> Matrix a
expandTo d (M _ m) = M d m

get :: Num a => Pos -> Matrix a -> a
get pos m = Map.findWithDefault 0 pos $ matrixOf m

set :: (Eq a, Num a) => Pos -> a -> Matrix a -> Matrix a
set pos v (M d m) = M (maxp pos d) m'
  where m' = if v == 0 then Map.delete pos m else Map.insert pos v m

del :: Pos -> Matrix a -> Matrix a
del pos m = m { matrixOf = Map.delete pos $ matrixOf m }

-- remove row/col (both?)

count :: Matrix a -> Int
count = Map.size . matrixOf

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add (M dl ml) (M dr mr) = M (maxp dl dr) $ Map.unionWith (+) ml mr

sub :: (Num a) => Matrix a -> Matrix a -> Matrix a
sub (M dl ml) (M dr mr) = M (maxp dl dr) $ Map.unionWith (-) ml mr

mul :: (Num a) => Matrix a -> Matrix a -> Matrix a
mul (M (r, _) ml) (M (_, c) mr) =
  -- not the most efficient algorithm but very concise
  M (r, c) $ Map.fromListWith (+) $
  [((rl,cr), vl*vr) | ((rl,cl), vl) <- ps ml, ((rr,cr), vr) <- ps mr, cl==rr]
  where ps = Map.toList

filter :: (a -> Bool) -> Matrix a -> Matrix a
filter f (M d m) = M d $ Map.filter f m

