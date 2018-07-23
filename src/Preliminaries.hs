module Preliminaries where

import Data.Fixed (mod')
import qualified Data.Map as M

type Interval a = (a, a)

interval :: a -> a -> Interval a
interval = (,)

intervalLength :: Num a => Interval a -> a
intervalLength (a, b) = b - a

contains :: Ord a => Interval a -> a -> Bool
contains (a, b) x = a <= x && x < b

union :: Ord a => Interval a -> Interval a -> Interval a
union (a, b) (c, d) = (min a c, max b d)

wrapInto :: Real a => Interval a -> a -> a
wrapInto (a, b) x
  | (a, b) `contains` x = x
  | otherwise = a + x `mod'` (b - a)

class Maplike a where
  type Domain a
  type Codomain a
  at :: a -> Domain a -> Codomain a

class Maplike a => InverseMaplike a where
  inv :: a -> Codomain a -> Domain a

class HasBounds a where
  type Bounds a
  bounds :: a -> Interval (Bounds a)

instance Ord k => Maplike (M.Map k v) where
  type Domain (M.Map k v) = k
  type Codomain (M.Map k v) = v
  at m k = case M.lookup k m of
    Just v -> v
    Nothing -> error "[Preliminaries] Map.at: key not present"
