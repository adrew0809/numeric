module Numeric.Roots (bisect,fixedPoint,secant,solve) where

import Control.Monad.State

solve :: (a -> a -> Bool) -> Int -> State b a -> b -> Maybe a
solve stop n step s0 = case solutions of
                         []        -> Nothing
                         ((p,_):_) -> Just p
     where solutions = dropWhile (not.stop') $ diffs
           stop'     = uncurry stop
           diffs     = zip ps ps'
           ps        = evalState (replicateM n step) s0
           ps'       = tail ps

bisect :: (Eq a,Fractional a) => (a -> a) -> State (a,a) a
bisect f = do (a,b) <- get
              let s' | signAt p == signAt a = (p,b)
                     | otherwise            = (a,p)
                  signAt                    = signum.f
                  p                         = (a + b) / 2
              put s'
              return p

fixedPoint :: (a -> a) -> State a a
fixedPoint f = state $ \p -> let p' = f p in (p', p')

secant :: Fractional a => (a -> a) -> State (a,a) a
secant f = do (p,p') <- get
              let fp   = f p
                  fp'  = f p'
                  p''  = (p*fp' - p'*fp)/(fp' - fp)
              put (p',p'')
              return p''

