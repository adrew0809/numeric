module Test.QuickCheck.Rational (
  chooseExclusiveRational,
  chooseRational,
  nonZeroRational,
  positiveRational,
  rational,
  rationalInterval,
  ) where

import Test.QuickCheck
import Data.Ratio

rational :: Gen Rational
rational = do d <- arbitrary
              n <- scale square arbitrary
              return $ n % (getNonZero d)

nonZeroRational :: Gen Rational
nonZeroRational = do d <- arbitrary
                     n <- scale square arbitrary
                     return $ (getNonZero n) % (getNonZero d)

positiveRational :: Gen Rational
positiveRational = do d <- arbitrary
                      n <- scale square arbitrary
                      return $ (getPositive n) % (getPositive d)

rationalInterval :: Gen (Rational,Rational)
rationalInterval = do a <- rational
                      d <- positiveRational
                      return (a,(a + d))

chooseRational :: (Rational,Rational) -> Gen Rational
chooseRational (a,b) = do s <- getSize
                          let d = denominator dist
                              n = numerator dist
                              s' = fromIntegral $ s + 1
                              dist = b - a
                          m <- chooseInteger(0, s')
                          return $ a + ((m * n) % (s' * d))

chooseExclusiveRational :: (Rational,Rational) -> Gen Rational
chooseExclusiveRational (a,b) = do s <- getSize
                                   let d = denominator dist
                                       n = numerator dist
                                       s' = fromIntegral $ s + 2
                                       dist = b - a
                                   m <- chooseInteger(1, s' - 1)
                                   return $ a + ((m * n) % (s' * d))

square :: Int -> Int
square = (^(2::Int))

