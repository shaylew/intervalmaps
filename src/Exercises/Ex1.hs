module Exercises.Ex1 where

import Data.List (findIndex)
import Data.Foldable (foldl')
import qualified Data.Map as M

import Preliminaries
import qualified Permutation as P
import qualified Enumeration as E
import qualified IntervalExchange as IEM

permutationPeriod :: P.Permutation -> Int
permutationPeriod = foldl' lcm 1 . map length . P.toCycles

permutationPeriodSimple :: P.Permutation -> Int
permutationPeriodSimple p = 1 + length (takeWhile (/= P.identity) powers)
  where
    powers = iterate (p <>) p

intersectIntervals :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersectIntervals (la, ra) (lb, rb)
  | l < r = Just (l, r)
  | otherwise = Nothing
  where
    l = max la lb
    r = min ra rb

example_f :: IEM.IEM Rational Char
example_f = IEM.IEM alphabet p0 p1 lengths
  where
    alphabet = E.fromList ['A', 'B', 'C']
    p0 = P.identity
    p1 = P.fromList [2, 0, 1]
    lengths = M.fromList [('A', 1/3), ('B', 1/5), ('C', 1/2)]

testPositions :: [Rational]
testPositions = [0, 0.2, 0.4, 0.6, 0.8, 1.0]

f_results :: [Rational]
f_results = map (IEM.evaluate example_f) testPositions

canonicalCompose :: (Real k, Ord a, Ord b) => IEM.IEM k a -> IEM.IEM k b -> IEM.IEM k Int
canonicalCompose f g = IEM.canonicalize (IEM.compose f g)

iteratedLengths :: (Real k, Ord a) => IEM.IEM k a -> [[k]]
iteratedLengths f = map lengths (iterate (canonicalCompose f) (IEM.canonicalize f))
  where
    lengths = M.elems . IEM.intervalLengths

approximateReturnPeriod :: (Real k, Ord a) => IEM.IEM k a -> k -> k -> Int
approximateReturnPeriod f x r =
  let ys = tail (iterate (IEM.evaluate f) x) in
  case findIndex (\y -> abs (x - y) < r) ys of
    Just i -> i + 1
    Nothing -> error "impossible, this list is infinite"

labelSequence :: (Real k, Ord a) => IEM.IEM k a -> k -> [a]
labelSequence f x = map (at intervals) (iterate (at f) x)
  where
    intervals = IEM.inputIntervals f

positionSimilarity :: (Real k, Ord a) => IEM.IEM k a -> k -> k -> Int -> Maybe Int
positionSimilarity f x y n = findIndex unequal (take n (zip xs ys))
  where
    unequal (a, b) = a /= b
    xs = labelSequence f x
    ys = labelSequence f y
