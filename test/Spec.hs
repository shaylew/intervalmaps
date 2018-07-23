import Test.Tasty
import Test.Tasty.QuickCheck

import Preliminaries
import qualified Permutation as P
import qualified IntervalExchange as IE

main :: IO ()
main = defaultMain properties

groupProperties
  :: forall g prop. (Testable prop, Arbitrary g, Show g)
  => (g -> g -> prop) -> g -> (g -> g) -> (g -> g -> g)
  -> TestTree
groupProperties equiv unit inverse op =
  testGroup "group laws"
  [ testProperty "left inverse" $ \x ->
      (inverse x `op` x) `equiv` unit
  , testProperty "right inverse" $ \x ->
      (x `op` inverse x) `equiv` unit
  , testProperty "left identity" $ \x ->
      (unit `op` x) `equiv` x
  , testProperty "right identity" $ \x ->
      (x `op` unit) `equiv` x
  , testProperty "associative" $ \x y z ->
      ((x `op` y) `op` z) `equiv` (x `op` (y `op` z))
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ permutationProps
  , intervalExchangeProps
  ]

permutationProps :: TestTree
permutationProps = testGroup "Permutation"
  [permGroupProps, permOperationProps]

permGroupProps :: TestTree
permGroupProps = groupProperties (==) P.identity P.inverse P.compose

permOperationProps :: TestTree
permOperationProps = testGroup "operations"
  [ testProperty "apply composes" P.prop_actionCompose
  , testProperty "toCycles/fromCycles" P.prop_toFromCycles
  ]

intervalExchangeProps :: TestTree
intervalExchangeProps = testGroup "IntervalExchange"
  [iemBasicProps, iemCanonicalizeProps, iemGroupProps]

type TestField = Int
type TestLabel = Int
type TestIEM = IE.IEM TestField TestLabel

iemBasicProps :: TestTree
iemBasicProps = testGroup "basic properties"
  [ testProperty "output in bounds" (IE.prop_outputInBounds @TestField @TestLabel)
  , testProperty "averages to zero" (IE.prop_averageZero @TestField @TestLabel)
  , testProperty "composition works" (IE.prop_composeSane @TestField @TestLabel)
  , testProperty "rename labels" (IE.prop_relabelSane @TestField)
  , testProperty "permute alphabet" (IE.prop_permuteSane @TestField @TestLabel)
  ]

iemCanonicalizeProps :: TestTree
iemCanonicalizeProps = testGroup "canonicalization"
  [ testProperty "preserves mapping" (IE.prop_canonicalSane @TestField @TestLabel)
  , testProperty "is stable" (IE.prop_canonicalStable @TestField @TestLabel)
  , testProperty "annihilates p0" (IE.prop_canonicalInputs @TestField @TestLabel)
  , testProperty "orders alphabet" (IE.prop_canonicalAlphabet @TestField @TestLabel)  
  ]

iemGroupProps :: TestTree
iemGroupProps = groupProperties @TestIEM equivalent identity IE.inverse canonicalCompose
  where
    equivalent :: TestIEM -> TestIEM -> TestField -> Bool
    equivalent f g = \x ->
      let y = wrapInto (bounds g) x
      in f `at` y == g `at` y
    identity = IE.canonicalize (IE.identity 10)
    canonicalCompose f g = IE.canonicalize (IE.compose f g)
