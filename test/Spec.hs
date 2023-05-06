import Numeric.Roots.Tests
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" 
  [ bisectProps, fixedPointProps, falsePositionProps]

bisectProps :: TestTree
bisectProps = testGroup "Bisection" 
  [ testProperty "f(a_n)f(b_n) < 0 implies f(a_n+1)f(b_n+1) < 0"
                 prop_bisectContainsRoot,
    testProperty "2(b_n+1 - a_n+1) = b_n - a_n"
                 prop_bisectHalves,
    testProperty "|p - p_n| <= (b - a)/2^n for 1st degree polynomial"
                 prop_bisectConverges
  ]

fixedPointProps :: TestTree
fixedPointProps = testGroup "Fixed Point" 
  [ testProperty "x in [a,b], f(x) in [a,b], 0 < k < 1, |f'(x)| < k in (a,b),\
                 \ then |p_n - p| <= k^n max{p_0 - a, b - p_0} for 1st degree\
                 \ polynomial"
                 prop_fixedPointConverges
  ]

falsePositionProps :: TestTree
falsePositionProps = testGroup "False Position" 
  [ testProperty "f(p_i)f(p_i-1) < 0 implies f(a_i+1)f(b_i) < 0"
                 prop_falsePositionContainsRoot
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ bisectTests, fixedPointTests, secantTests, newtonTests, falsePositionTests
  ]

bisectTests :: TestTree
bisectTests = testGroup "Bisection"
  [ testCase "f(x) = x^3 + 4x^2 - 10, a_0 = 1, b_0 = 2, n = 10, converges to\
             \ p within 10e-3"
             test_bisect
  ]

fixedPointTests :: TestTree
fixedPointTests = testGroup "Fixed Point"
  [ testCase "f(x) = x - (x^3 + 4x^2 - 10)/(3x^2 + 8x), p0 = 1.5, n=10,\
             \ converges to p within 10e-3"
             test_fixedPoint,
    testCase "f(x) = x - x^3 - 4x^2 + 10, p0 = 1.5, n=10, does not converge to\
             \ p within 10e-3"
             test_fixedPointDiverge
  ]

secantTests :: TestTree
secantTests = testGroup "Secant"
  [ testCase "x = cos x, p0 = 0.5, p1 = pi/4, n=5, converges to p within 10e-3"
             test_secant
  ]

newtonTests :: TestTree
newtonTests = testGroup "Newton"
  [ testCase "f(x) = cos x - x, f'(x) = -sinx -1, p0 = pi/4, n=4, converges to\
             \ p within 10e-3"
             test_newton
  ]

falsePositionTests :: TestTree
falsePositionTests = testGroup "False Position"
  [ testCase "x = cos x, p0 = 0.5, p1 = pi/4, n=6, converges to p within 10e-3"
             test_falsePosition
  ]
