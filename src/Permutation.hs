module Permutation (
  Permutation
  -- * Basic operations
  , apply
  , compose
  , inverse
  , size
  -- * Creation
  , identity
  , fromList
  , fromCycles
  , fromSwaps
  -- * Conversion
  , toList
  , toPairs
  , toCycles
  -- * Test helpers
  , arbitraryN
  -- * Properties (for QuickCheck)
  , prop_actionCompose
  , prop_toFromCycles
  ) where

import Control.Monad (forM, forM_)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Test.QuickCheck as QC

import Preliminaries

newtype Permutation = MkP (V.Vector Int)

apply :: Permutation -> Int -> Int
apply (MkP p) i = fromMaybe i (p V.!? i)

identity :: Permutation
identity = MkP V.empty

inverse :: Permutation -> Permutation
inverse (MkP p) = MkP (V.update p (V.map swap (V.indexed p)))
  where swap (a, b) = (b, a)

compose :: Permutation -> Permutation -> Permutation
compose p q = MkP (V.generate n (at p . at q))
  where n = max (size p) (size q)

size :: Permutation -> Int
size (MkP p) = V.length p

fromList :: [Int] -> Permutation
fromList xs
  | isPermutation xs = MkP (V.fromList xs)
  | otherwise = error "fromList: not a valid permutation"

isPermutation :: [Int] -> Bool
isPermutation xs = sort xs == [0 .. length xs - 1]

fromCycle :: [Int] -> Permutation
fromCycle [] = identity
fromCycle xs = MkP (identityN V.// pairs)
  where
    n = maximum xs + 1
    identityN = V.generate n id
    pairs = zip xs (tail xs ++ [head xs])

fromCycles :: [[Int]] -> Permutation
fromCycles = foldl' compose identity . map fromCycle

fromSwapsN :: Int -> [(Int, Int)] -> Permutation
fromSwapsN n pairs = MkP $ V.create $ do
  v <- MV.new n
  forM_ [0 .. n - 1] $ \i ->
    MV.write v i i
  forM_ pairs $ \(i, j) ->
    MV.swap v i j
  return v

fromSwaps :: [(Int, Int)] -> Permutation
fromSwaps pairs = fromSwapsN n pairs
  where
    n = maximum (concat [[0], map fst pairs, map snd pairs])

toList :: Permutation -> [Int]
toList (MkP v) = V.toList v

toPairs :: Permutation -> [(Int, Int)]
toPairs = zip [0..] . toList

toCycles :: Permutation -> [[Int]]
toCycles p = loop [0 .. size p - 1] S.empty
  where
    loop [] _ = []
    loop (x:xs) visited
      | x `S.member` visited = loop xs visited
      | otherwise = orbit : loop xs newVisited
      where
        orbit = x : takeWhile (/= x) (tail (iterate (at p) x))
        newVisited = S.union visited (S.fromList orbit)

instance Eq Permutation where
  (==) p q = all (\i -> at p i == at q i) [0..n]
    where n = max (size p) (size q)

instance Semigroup Permutation where
  (<>) = compose

instance Monoid Permutation where
  mempty = identity

instance HasBounds Permutation where
  type Bounds Permutation = Int
  bounds (MkP p) = (0, V.length p)

instance Maplike Permutation where
  type Domain Permutation = Int
  type Codomain Permutation = Int
  at = apply

instance InverseMaplike Permutation where
  inv (MkP p) i = fromMaybe i (V.findIndex (== i) p)

instance Show Permutation where
  showsPrec d (MkP p) =
    showParen (d > 5) (showString "fromList " . showsPrec 6 (V.toList p))

instance QC.Arbitrary Permutation where
  arbitrary = do
    n <- QC.arbitrarySizedNatural
    arbitraryN n

arbitraryN :: Int -> QC.Gen Permutation
arbitraryN n = do
  swaps <- forM [0 .. n - 1] $ \i -> do
    j <- QC.choose (i, n - 1)
    return (i, j)
  return (fromSwapsN n swaps)

prop_actionCompose :: Permutation -> Permutation -> Int -> Bool
prop_actionCompose p q i = (at p . at q) i == at (p <> q) i

prop_toFromCycles :: Permutation -> Bool
prop_toFromCycles p = p == fromCycles (toCycles p)
