{-# LANGUAGE DeriveFunctor #-}

module BespokeMonad (Computation(..), ioProgram, program) where

{-
 - This example solves the challenge with a single manually constructed monad.
 - This is as basic as it gets: no monad transformers, no free monads, no
 - finally tagless encodings, etc.
 -}

import Control.Monad (replicateM_)
import System.Random (StdGen, mkStdGen, randomR)

newtype Computation a = Computation {
    runComputation :: StdGen -> Integer -> (StdGen, Integer, String, a)
  } deriving Functor

instance Applicative Computation where
  pure x = Computation $ \g i -> (g, i, "", x)
  f <*> c = Computation $ \g1 i1 ->
    let (g2, i2, s2, x2) = runComputation f g1 i1
        (g3, i3, s3, x3) = runComputation c g2 i2
        x4 = x2 x3
    in (g3, i3, s2 ++ s3, x4)

instance Monad Computation where
  c >>= f = Computation $ \g1 i1 ->
    let (g2, i2, s2, x2) = runComputation c g1 i1
        (g3, i3, s3, x3) = runComputation (f x2) g2 i2
    in (g3, i3, s2 ++ s3, x3)

getRandom :: Computation Integer
getRandom =
  Computation $ \g1 i -> let (r, g2) = randomR (0, 9) g1 in (g2, i, "", r)

getAccumulator :: Computation Integer
getAccumulator = Computation $ \g i -> (g, i, "", i)

setAccumulator :: Integer -> Computation ()
setAccumulator i = Computation $ \g _ -> (g, i, "", ())

logOutput :: String -> Computation ()
logOutput s = Computation $ \g i -> (g, i, s, ())

run :: Computation a -> IO a
run (Computation k) =
  let (_, _, s, x) = k (mkStdGen 0) 0 in putStrLn s >> return x

program :: Computation ()
program = replicateM_ 10 $ do
  i <- getAccumulator
  logOutput (show i ++ "\n")
  r <- getRandom
  setAccumulator (r + i)
  return ()

ioProgram :: IO ()
ioProgram = run program
