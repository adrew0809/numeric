module Numeric.Roots.Tests (
  prop_bisectContainsRoot,
  prop_bisectConverges,
  prop_bisectHalves,
  prop_fixedPointConverges,
  test_bisect,
  test_fixedPoint,
  test_fixedPointDiverge,
  test_secant,
  ) where

import Numeric.Roots (bisect,fixedPoint,secant,solve)
import Test.HUnit (Assertion,(@?),assert,assertFailure)
import Test.QuickCheck
import Test.QuickCheck.Rational
import Control.Monad.State (evalState,execState,replicateM)

prop_bisectContainsRoot :: Fun Rational Rational -> (Rational,Rational) ->
                           Property
prop_bisectContainsRoot (Fun _ f) (a,b) =
  let (a',b') = execState (bisect f) (a,b)
  in signum (f a) /= signum (f b) ==> signum (f a') =/= signum (f b')

prop_bisectHalves :: Fun Rational Rational -> Rational -> Rational -> Property
prop_bisectHalves (Fun _ f) a b = let (a',b') = execState (bisect f) (a,b)
                                  in 2*abs (b' - a') === abs (b - a)

prop_bisectConverges :: Positive Int -> Interval Rational -> Rational ->
                        Property
prop_bisectConverges (Positive n) (Interval (a,b)) m = 
    forAll (chooseRational (a,b)) $ \p ->
    let f x = m*(x - p) -- polynomial with root p
        p'  = last $ evalState (replicateM n (bisect f)) (a,b)
    in abs (p - p') <= (b - a) / 2^n

test_bisect :: Assertion
test_bisect = case solve stop n (bisect f) s0 of
              Nothing -> error "Failed to converge"
              Just x -> abs (x - x') < t @? "Result not near root"
  where x'        = 1.365230013::Double
        stop p p' = abs (p - p') < t
        t         = 10e-3
        n         = 10
        f x       = x**3 + 4*x**2 - 10
        s0        = (1,2)

prop_fixedPointConverges :: (Positive Int) -> Interval Rational -> Property
prop_fixedPointConverges (Positive n) (Interval (a,b)) =
  forAll (chooseRational (a,b)) $ \p ->
  forAll (chooseRational (a,b)) $ \p0 ->
  forAll (chooseExclusiveRational (0,1)) $ \k ->
  forAll (chooseRational (-k,k)) $ \m ->
  let f x = m*x + (1 - m)*p -- polynomial with fixed point p
      pn  = last $ evalState (replicateM n (fixedPoint f)) p0
  in abs (p - pn) <= k^^n*(max (p0 - a) (b - p0))

test_fixedPoint :: Assertion
test_fixedPoint = case solve stop n (fixedPoint f) s0 of
                  Nothing -> assertFailure "Failed to converge"
                  Just x  -> abs (x - x') < t @? "Result not near root"
  where x'        = 1.365230013::Double
        stop p p' = abs (p - p') < t
        t         = 10e-3
        n         = 10
        f x       = x - (x**3 + 4*x**2 - 10)/(3*x**2 + 8*x)
        s0        = 1.5

test_fixedPointDiverge :: Assertion
test_fixedPointDiverge = case solve stop n (fixedPoint f) s0 of
                         Nothing -> assert True
                         Just _  -> assertFailure "Converged"
  where stop p p' = abs (p - p') < t
        t         = 10e-3
        n         = 10
        f x       = (x - x**3 - 4*x**2 + 10)
        s0        = 1.5::Double

test_secant :: Assertion
test_secant = case solve stop n (secant f) s0 of
                  Nothing -> assertFailure "Failed to converge"
                  Just x  -> abs (x - x') < t @? "Result not near root"
  where x'        = 0.7390851332::Double
        stop p p' = abs (p - p') < t
        t         = 10e-3
        n         = 5
        f x       = cos x - x
        s0        = (0.5, pi/4)
