module Enumeration where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC

import Preliminaries
import qualified Permutation as P

data Enumeration a = Enumeration
  { toInt :: M.Map a Int
  , fromInt :: M.Map Int a
  } deriving (Eq)

size :: Enumeration a -> Int
size e = M.size (toInt e)

elements :: Enumeration a -> [a]
elements e = M.elems (fromInt e)

elementSet :: Enumeration a -> S.Set a
elementSet = M.keysSet . toInt

indices :: Enumeration a -> [Int]
indices e = [0 .. size e - 1]

orderedBy :: P.Permutation -> Enumeration a -> [a]
orderedBy p e = map (at e . at p) (indices e)

fromList :: Ord a => [a] -> Enumeration a
fromList xs
  | M.size toI == M.size fromI = Enumeration toI fromI
  | otherwise = error "Enumeration.fromList: elements not unique"
  where
    toI = M.fromList (zip xs [0..])
    fromI = M.fromList (zip [0..] xs)

instance HasBounds (Enumeration a) where
  type Bounds (Enumeration a) = Int
  bounds (Enumeration _ m) = (fst (M.findMin m), fst (M.findMax m))

instance Maplike (Enumeration a) where
  type Domain (Enumeration a) = Int
  type Codomain (Enumeration a) = a
  at e i = case M.lookup i (fromInt e) of
    Just a -> a
    Nothing -> error "Enumeration.at: index out of bounds"

instance Ord a => InverseMaplike (Enumeration a) where
  inv e a = case M.lookup a (toInt e) of
    Just i -> i
    Nothing -> error "Enumeration.inv: element not present"

instance Show a => Show (Enumeration a) where
  showsPrec d e = showParen (d > 5) (showString "fromList " . showsPrec 6 vs)
    where
      vs = M.elems (fromInt e)

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (Enumeration a) where
  arbitrary = fmap fromList QC.arbitrary
