{-# LANGUAGE DeriveFunctor #-}

module BespokeMonad
  ( interpret
  , program
  ) where

{-
 - This example solves the challenge with a handcrafted monad. This is as basic
 - as it gets: no monad transformers, no free monads, no extensible effects,
 - etc. The use of monads to describe the denotational semantics of effectful
 - programs was first described in [1]. In the following year, [2] showed how
 - monads could be used to structure programs rather than reason about them.
 -
 - [1] E. Moggi. 1989. Computational lambda-calculus and monads. In Proceedings
 -     of the Fourth Annual Symposium on Logic in computer science. IEEE Press,
 -     Piscataway, NJ, USA, 14-23.
 -
 - [2] Philip Wadler. 1990. Comprehending monads. In Proceedings of the 1990
 -     ACM conference on LISP and functional programming (LFP '90). ACM, New
 -     York, NY, USA, 61-78. DOI=http://dx.doi.org/10.1145/91556.91592
 -}
import Control.Monad (ap, replicateM_)
import System.Random (StdGen, mkStdGen, randomR)

-- The monad
newtype Computation a =
  Computation
    { runComputation :: StdGen -> Integer -> (StdGen, Integer, String, a)
    }
  deriving (Functor)

instance Applicative Computation where
  pure x = Computation $ \g i -> (g, i, "", x)
  (<*>) = ap

instance Monad Computation where
  c >>= f =
    Computation $ \g1 i1 ->
      let (g2, i2, s2, x2) = runComputation c g1 i1
          (g3, i3, s3, x3) = runComputation (f x2) g2 i2
       in (g3, i3, s2 ++ s3, x3)

-- The operations
getRandom :: Computation Integer
getRandom =
  Computation $ \g1 i ->
    let (r, g2) = randomR (0, 9) g1
     in (g2, i, "", r)

getAccumulator :: Computation Integer
getAccumulator = Computation $ \g i -> (g, i, "", i)

setAccumulator :: Integer -> Computation ()
setAccumulator i = Computation $ \g _ -> (g, i, "", ())

logOutput :: String -> Computation ()
logOutput s = Computation $ \g i -> (g, i, s, ())

-- The program
program :: Computation ()
program =
  replicateM_ 10 $ do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)

-- An interpreter
interpret :: Computation a -> (a, String)
interpret (Computation k) =
  let (_, _, o, x) = k (mkStdGen 0) 0
   in (x, o)
