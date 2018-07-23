module IntervalMap where

import Prelude hiding (zip)
import qualified Prelude (zip)

import Preliminaries
import qualified Data.Vector as V

data IntervalMap a b = IM
  { imRight :: a
  , imSteps :: V.Vector (a, b)
  } deriving (Eq, Show)

fromSteps :: Ord a => [(a, b)] -> a -> IntervalMap a b
fromSteps steps rightBound
  | not inBounds = error "fromSteps: right bound not consistent with steps"
  | not sorted = error "fromSteps: steps must be strictly ascending"
  | otherwise = IM rightBound (V.fromList steps)
  where
    inBounds = True -- all (\(x, _) -> x < rightBound) steps
    leftBounds = map fst steps
    sorted = and (zipWith (<) leftBounds (tail leftBounds))

fromLengths :: (Num a, Ord a) => a -> [(a, b)] -> IntervalMap a b
fromLengths leftBound pairs
  | any (<= 0) lengths = error "fromLengths: lengths must be strictly positive"
  | otherwise = IM rightBound (V.fromList (Prelude.zip leftBounds vs))
  where
    (lengths, vs) = unzip pairs
    points = scanl (+) leftBound lengths
    (leftBounds, rightBound) = (init points, last points)

imLeft :: IntervalMap a b -> a
imLeft (IM right steps) = case steps V.!? 0 of
  Just (x, _) -> x
  Nothing -> right

indexAt :: Ord a => IntervalMap a b -> a -> Maybe Int
indexAt (IM right steps) x
  | x >= right = Nothing
  | otherwise = loop (V.length steps - 1)
  where
    -- we could binary search here if we felt like it
    loop i
      | i < 0 = Nothing
      | fst (steps V.! i) <= x = Just i
      | otherwise = loop (i - 1)

numIntervals :: IntervalMap a b -> Int
numIntervals m = V.length (imSteps m)

nthInterval :: IntervalMap a b -> Int -> Interval a
nthInterval (IM rightBound steps) i = (left, right)
  where
    (left, _) = steps V.! i
    right = case steps V.!? (i + 1) of
      Just (x, _) -> x
      Nothing -> rightBound

nthValue :: IntervalMap a b -> Int -> b
nthValue (IM _ steps) i = snd (steps V.! i)

intervalAt :: Ord a => IntervalMap a b -> a -> Maybe (Interval a)
intervalAt m a = fmap (nthInterval m) (indexAt m a)

toPairs :: IntervalMap a b -> [(Interval a, b)]
toPairs (IM r v) = zipWith3 (\x y a -> ((x, y), a)) lefts rights vs
  where
    (lefts, vs) = unzip (V.toList v)
    rights = tail lefts ++ [r]

intervals :: IntervalMap a b -> [Interval a]
intervals = map fst . toPairs

values :: IntervalMap a b -> [b]
values = map snd . toPairs

instance Ord a => Maplike (IntervalMap a b) where
  type Domain (IntervalMap a b) = a
  type Codomain (IntervalMap a b) = b

  at :: IntervalMap a b -> a -> b
  at m a = case indexAt m a of
    Just i -> nthValue m i
    Nothing -> error "IntervalMap: point not in bounds"

instance HasBounds (IntervalMap a b) where
  type Bounds (IntervalMap a b) = a
  bounds m = (imLeft m, imRight m)

instance Functor (IntervalMap a) where
  fmap f (IM r v) = IM r (V.map (fmap f) v)

zip :: Ord a => IntervalMap a b -> IntervalMap a c -> IntervalMap a (b, c)
zip p1 p2 = IM rightBound steps
  where
    leftBound = max (imLeft p1) (imLeft p2)
    rightBound = min (imRight p1) (imRight p2)
    points1 = V.toList (V.map fst (imSteps p1))
    points2 = V.toList (V.map fst (imSteps p2))
    points = filter (\x -> leftBound <= x && x < rightBound) (mergeUnique points1 points2)
    steps = V.fromList (map (\p -> (p, (p1 `at` p, p2 `at` p))) points)

mergeUnique :: Ord a => [a] -> [a] -> [a]
mergeUnique xs [] = xs
mergeUnique [] ys = ys
mergeUnique (x:xs) (y:ys) = case compare x y of
  LT -> x : mergeUnique xs (y:ys)
  EQ -> x : mergeUnique xs ys
  GT -> y : mergeUnique (x:xs) ys
