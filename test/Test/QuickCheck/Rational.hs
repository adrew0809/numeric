module Test.QuickCheck.Rational (
  chooseExclusiveRational,
  chooseRational,
  Interval(..),
  ) where

import Test.QuickCheck

newtype Interval a = Interval (a,a)
                     deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (Interval a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Interval $ if a < b
                                     then (a,b)
                                     else (b,a)

chooseRational :: (Rational,Rational) -> Gen Rational
chooseRational (a,b) = do s <- getSize
                          x <- chooseInt (-s,s)
                          let x' = fromIntegral x
                              s' = fromIntegral s
                          return $ mapOnto x' (-s',s') (a,b)

chooseExclusiveRational :: (Rational,Rational) -> Gen Rational
chooseExclusiveRational (a,b) = do s <- getSize
                                   x <- chooseInt ((-s) + 1,s - 1)
                                   let x' = fromIntegral x
                                       s' = fromIntegral s
                                   return $ mapOnto x' (-s',s') (a,b)

mapOnto :: (Eq a, Fractional a) => a -> (a,a) -> (a,a) -> a
mapOnto x (a,b) (a',b') = if s == 0
                          then a
                          else (b' - a')*x/(2*s) + (a' + b')/2
                where s = b - a
