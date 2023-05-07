{-|
Module: Numeric.Roots
Description: Root finding algorithms
Copyright: (c) A.E. Drew, 2023
-}
module Numeric.Roots (
-- * Runner
  solve,
-- * Iterative steps
  bisect,
  falsePosition,
  fixedPoint,
  newton,
  secant,
  ) where

import Control.Monad.State

-- | Runs an iterative algorithm until it satisfies a stopping condition or
-- performs n iterations. The stopping condition takes the last two
-- approxiamtions.
solve :: (a -> a -> Bool) -> Int -> State b a -> b -> Maybe a
solve stop n step s0 = case solutions of
                         []        -> Nothing
                         ((p,_):_) -> Just p
     where solutions = dropWhile (not.stop') $ diffs
           stop'     = uncurry stop
           diffs     = zip ps ps'
           ps        = evalState (replicateM n step) s0
           ps'       = tail ps

-- | \[
-- p_n = \frac{a_n + b_n}{2} \\
-- (a_n,b_n) = \begin{cases} 
--               (p_{n-1},b_{n-1}) & \quad \text{if } f(a_{n-1})f(p) < 0 \\
--               (a_{n-1}, p_{n-1})  & \quad \text{else}
--             \end{cases}
-- \]
bisect :: (Eq a,Fractional a) => (a -> a) -> State (a,a) a
bisect f = do (a,b) <- get
              let s' | signAt p == signAt a = (p,b)
                     | otherwise            = (a,p)
                  signAt                    = signum.f
                  p                         = (a + b) / 2
              put s'
              return p

-- | \[
-- p_n = f(p_{n-1})
-- \]
fixedPoint :: (a -> a) -> State a a
fixedPoint f = state $ \p -> let p' = f p in (p', p')

-- | \[
-- p_n = \frac{p_{n-2}f(p_{n-1}) - p_{n-1}f(p_{n-2})}{f(p_{n-1}) - f(p_{n-2})}
-- \]
secant :: Fractional a => (a -> a) -> State (a,a) a
secant f = do (p,p') <- get
              let p'' = calcSecant f p p'
              put (p',p'')
              return p''

-- | \[
-- p_n = p_{n-1} - \frac{f(p_{n-1})}{f'(p_{n-1})}
-- \]
newton :: Fractional a => (a -> a) -> (a -> a) -> State a a
newton f  f' = fixedPoint $ \p -> p - (f p)/(f' p)

-- | \[
-- p_n = \frac{p_{n-2}f(p_{n-1}) - p_{n-1}f(p_{n-2})}
--            {f(p_{n-1}) - f(p_{n-2})} \\
-- p_{n-1} = \begin{cases} 
--             p_{n-1} & \quad \text{if } f(p_n)f(p_{n-1}) < 0 \\
--             p_{n-2} & \quad \text{else}
--           \end{cases}
-- \]
falsePosition :: (Eq a, Fractional a) => (a -> a) -> State (a,a) a
falsePosition f = do (p,p') <- get
                     let p''  = calcSecant f p p'
                     if signum (f p'') == signum (f p')
                     then put (p,p'')
                     else put (p',p'')
                     return p''

calcSecant :: Fractional a => (a -> a) -> a -> a -> a
calcSecant f p p' = (p*fp' - p'*fp)/(fp' - fp)
        where fp  = f p
              fp' = f p'
