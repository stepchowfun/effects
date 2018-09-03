{-# LANGUAGE DeriveFunctor #-}

module BespokeMonad
  ( Computation(..)
  , ioProgram
  , program
  ) where

{-
 - This example solves the challenge with a single manually constructed monad.
 - This is as basic as it gets: no monad transformers, no free monads, no
 - final tagless encodings, etc.
 -}
import Control.Monad (ap, replicateM_)
import System.Random (StdGen, mkStdGen, randomR)

-- The monad
newtype Computation a = Computation
  { runComputation :: StdGen -> Integer -> (StdGen, Integer, String, a)
  } deriving (Functor)

instance Applicative Computation where
  pure = return
  (<*>) = ap

instance Monad Computation where
  return x = Computation $ \g i -> (g, i, "", x)
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
    return ()

-- An interpreter
interpret :: Computation a -> IO a
interpret (Computation k) =
  let (_, _, s, x) = k (mkStdGen 0) 0
  in putStrLn s >> return x

-- An interpretation of the program
ioProgram :: IO ()
ioProgram = interpret program
