{-# LANGUAGE DeriveFunctor, LambdaCase #-}

module FreeMonad (interpret, ioProgram, program) where

{-
 - This example solves the challenge with a free monad.
 -}

import Control.Monad (replicateM_)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Control.Monad.Random (getRandomR)
import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import qualified MonadTransformers

data Operations a = GetRandom (Integer -> a)
                  | GetAccumulator (Integer -> a)
                  | SetAccumulator Integer a
                  | LogOutput String a
  deriving Functor

type Computation = Free Operations

getRandom :: Computation Integer
getRandom = Free (GetRandom return)

getAccumulator :: Computation Integer
getAccumulator = Free (GetAccumulator return)

setAccumulator :: Integer -> Computation ()
setAccumulator i = liftF (SetAccumulator i ())

logOutput :: String -> Computation ()
logOutput s = liftF (LogOutput s ())

interpret :: Computation a -> MonadTransformers.Computation a
interpret = foldFree $ \case
  GetRandom      k   -> k <$> getRandomR (0, 9)
  GetAccumulator k   -> k <$> get
  SetAccumulator i k -> k <$ put i
  LogOutput      s k -> k <$ tell s

program :: Computation ()
program = replicateM_ 10 $ do
  i <- getAccumulator
  logOutput (show i ++ "\n")
  r <- getRandom
  setAccumulator (r + i)
  return ()

ioProgram :: IO ()
ioProgram = MonadTransformers.interpret (interpret program)
