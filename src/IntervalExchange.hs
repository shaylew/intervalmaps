module IntervalExchange (
  IEM(..)
  -- * The basics
  , evaluate
  , identity
  , inverse
  , compose
  , equivalent
  -- * Useful properties
  , inputIntervals
  , outputIntervals
  , intervalTranslations
  , monodromy
  , rightBound
  -- * Transformations
  , relabel
  , permute
  , canonicalize
  -- * Test helpers
  , arbitraryIEM
  -- * Properties (for QuickCheck)
  , prop_outputInBounds
  , prop_averageZero
  , prop_composeSane
  , prop_relabelSane
  , prop_permuteSane
  , prop_canonicalSane
  , prop_canonicalStable
  , prop_canonicalInputs
  , prop_canonicalAlphabet
  
    ) where

import Control.Monad (replicateM)
import Data.List (sort)
import System.Random (Random)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Test.QuickCheck as QC

import Preliminaries
import qualified Enumeration as E
import qualified Permutation as P
import qualified IntervalMap as IM

data IEM k a = IEM
  { alphabet :: E.Enumeration a
  , inputOrder :: P.Permutation
  , outputOrder :: P.Permutation
  , intervalLengths :: M.Map a k
  } deriving (Show)

rightBound :: Real k => IEM k a -> k
rightBound = sum . M.elems . intervalLengths

inputIntervals :: (Real k, Ord a) => IEM k a -> IM.IntervalMap k a
inputIntervals m = IM.fromLengths 0 (map (\a -> (intervalLengths m `at` a, a)) labels)
  where
    labels = E.orderedBy (inputOrder m) (alphabet m)

outputIntervals :: (Real k, Ord a) => IEM k a -> IM.IntervalMap k a
outputIntervals m = IM.fromLengths 0 (map (\a -> (intervalLengths m `at` a, a)) labels)
  where
    labels = E.orderedBy (outputOrder m) (alphabet m)

intervalTranslations :: (Real k, Ord a) => IEM k a -> M.Map a k
intervalTranslations f = M.intersectionWith (-) outputPositions inputPositions
  where
    lengthOf x = intervalLengths f `at` x
    inputLabels = E.orderedBy (inputOrder f) (alphabet f)
    outputLabels = E.orderedBy (outputOrder f) (alphabet f)
    inputPositions = M.fromList (zip inputLabels (scanl (+) 0 (map lengthOf inputLabels)))
    outputPositions = M.fromList (zip outputLabels (scanl (+) 0 (map lengthOf outputLabels)))

monodromy :: IEM k a -> P.Permutation
monodromy m = outputOrder m <> P.inverse (inputOrder m)

identity :: k -> IEM k ()
identity k = IEM (E.fromList [()]) P.identity P.identity (M.fromList [((), k)])

inverse :: IEM k a -> IEM k a
inverse (IEM a p0 p1 ks) = IEM a p1 p0 ks

compose :: (Real k, Ord a, Ord b)
        => IEM k a -> IEM k b -> IEM k (b, a)
compose f g = IEM {
  alphabet = a,
  inputOrder = p0,
  outputOrder = p1,
  intervalLengths = lengths
  }
  where
    (intervals, labels) = unzip (IM.toPairs (IM.zip (outputIntervals g) (inputIntervals f)))
    a = E.fromList labels
    lengths = M.fromList (zip labels (map intervalLength intervals))
    g_groups = M.fromList (chunkBy fst labels)
    f_groups = M.fromList (chunkBy snd labels)
    p0 = (P.fromList . map (inv a) . expandBy g_groups) (E.orderedBy (inputOrder g) (alphabet g))
    p1 = (P.fromList . map (inv a) . expandBy f_groups) (E.orderedBy (outputOrder f) (alphabet f))

chunkBy :: Eq a => (b -> a) -> [b] -> [(a, [b])]
chunkBy _ [] = []
chunkBy f (x:xs) = (fx, x : group) : chunkBy f rest
  where
    fx = f x
    (group, rest) = span (\y -> f y == fx) xs

expandBy :: Ord a => M.Map a [b] -> [a] -> [b]
expandBy m xs = concatMap (\x -> M.findWithDefault [] x m) xs

evaluate :: (Real k, Ord a) => IEM k a -> k -> k
evaluate f = \x -> x + translations `at` (inputs `at` x)
  where
    inputs = inputIntervals f
    translations = intervalTranslations f

relabel :: (Ord b) => (a -> b) -> IEM k a -> IEM k b
relabel f m = IEM a (inputOrder m) (outputOrder m) lengths
  where
    a = (E.fromList . map f . E.elements) (alphabet m)
    lengths = M.mapKeys f (intervalLengths m)

permute :: Ord a => P.Permutation -> IEM k a -> IEM k a
permute p m = IEM a p0 p1 (intervalLengths m)
  where
    a = (E.fromList . E.orderedBy (P.inverse p)) (alphabet m)
    p0 = p <> inputOrder m
    p1 = p <> outputOrder m

canonicalize :: Ord a => IEM k a -> IEM k Int
canonicalize m = m2
  where
    m1 = permute (P.inverse (inputOrder m)) m
    m2 = relabel (inv (alphabet m1)) m1

equivalent :: (Eq k, Ord a) => IEM k a -> IEM k a -> Bool
equivalent f g = canonicalize f == canonicalize g

instance Real k => HasBounds (IEM k a) where
  type Bounds (IEM k a) = k
  bounds m = (0, rightBound m)

instance (Real k, Ord a) => Maplike (IEM k a) where
  type Domain (IEM k a) = k
  type Codomain (IEM k a) = k
  at = evaluate

instance (Real k, Ord a) => InverseMaplike (IEM k a) where
  inv = evaluate . inverse

instance (Eq k, Eq a) => Eq (IEM k a) where
  (==) f g = and
    [ E.elementSet (alphabet f) == E.elementSet (alphabet g)
    , monodromy f == monodromy g
    , intervalLengths f == intervalLengths g
    ]

instance (Real k, Random k, Ord a, QC.Arbitrary k, QC.Arbitrary a) => QC.Arbitrary (IEM k a) where
  arbitrary = do
    n <- QC.choose (1, 3)
    s <- return 10
    arbitraryIEM n s

arbitraryIEM
  :: (Real k, Random k, Ord a, QC.Arbitrary a)
  => Int -> k -> QC.Gen (IEM k a)
arbitraryIEM n s = do
    a <- (fmap (Set.elems . Set.fromList)) (QC.vector n)
    let m = length a
    p0 <- P.arbitraryN m
    p1 <- P.arbitraryN m
    lengths <- arbitraryPartition m s
    let lengthMap = M.fromList (zip a lengths)
    return (IEM (E.fromList a) p0 p1 lengthMap)

arbitraryPartition :: (Num a, Ord a, Random a) => Int -> a -> QC.Gen [a]
arbitraryPartition n s = do
  let s' = s - fromIntegral n
  xs <- replicateM (n - 1) (QC.choose (0, s'))
  let xs' = sort xs
  let ds = zipWith (-) (xs' ++ [s']) (0 : xs')
  return (map (+ 1) ds)

prop_outputInBounds :: forall k a. (Real k, Ord a) => IEM k a -> k -> Bool
prop_outputInBounds f x = bounds f `contains` evaluate f y
  where y = wrapInto (bounds f) x

prop_averageZero :: forall k a. (Real k, Ord a) => IEM k a -> Bool
prop_averageZero f = sum (M.intersectionWith (*) ts lengths) == 0
  where
    ts = intervalTranslations f
    lengths = intervalLengths f

prop_canonicalSane :: forall k a. (Real k, Ord a) => IEM k a -> k -> Bool
prop_canonicalSane f x = canonicalize f `at` y == f `at` y
  where y = wrapInto (bounds f) x

prop_canonicalStable :: forall k a. (Real k, Ord a) => IEM k a -> Bool
prop_canonicalStable f = canonicalize f == canonicalize (canonicalize f)

prop_canonicalInputs :: forall k a. (Ord a) => IEM k a -> Bool
prop_canonicalInputs f = inputOrder (canonicalize f) == P.identity

prop_canonicalAlphabet :: forall k a. (Ord a) => IEM k a -> Bool
prop_canonicalAlphabet f = and (zipWith (==) [0..] (E.elements (alphabet (canonicalize f))))

prop_composeSane :: forall k a. (Real k, Ord a) => IEM k a -> IEM k a -> k -> Bool
prop_composeSane f g x = (at f . at g) y == at (f `compose` g) y
  where y = wrapInto (bounds g) x

prop_relabelSane :: Real k => P.Permutation -> IEM k Int -> k -> Bool
prop_relabelSane p f x = relabel (at p) f `at` y == f `at` y
  where y = wrapInto (bounds f) x

prop_permuteSane :: (Real k, Ord a) => P.Permutation -> IEM k a -> k -> Bool
prop_permuteSane p f x = permute q f `at` y == f `at` y
  where
    n = E.size (alphabet f)
    y = wrapInto (bounds f) x
    q = P.fromCycles . map (filter (< n)) . P.toCycles $ p
