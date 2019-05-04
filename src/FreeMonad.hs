{-# LANGUAGE DeriveFunctor #-}

module FreeMonad
  ( interpret
  , program
  ) where

{-
 - This example solves the challenge with a free monad. The use of free monads
 - to structure programs was popularized by [1].
 -
 - [1] Wouter Swierstra. 2008. Data types à la carte. J. Funct. Program. 18, 4
 -     (July 2008), 423-436. DOI=http://dx.doi.org/10.1017/S0956796808006758
 -}
import Control.Monad (replicateM_)
import Control.Monad.Free (Free(..), foldFree, liftF)
import Control.Monad.Random (getRandomR)
import Control.Monad.State (gets, put)
import Control.Monad.Writer (tell)
import qualified MonadTransformers

-- The monad
type Computation = Free Operations

-- The operations
data Operations a
  = GetRandom (Integer -> a)
  | GetAccumulator (Integer -> a)
  | SetAccumulator Integer
                   a
  | LogOutput String
              a
  deriving (Functor)

getRandom :: Computation Integer
getRandom = liftF (GetRandom id)

getAccumulator :: Computation Integer
getAccumulator = liftF (GetAccumulator id)

setAccumulator :: Integer -> Computation ()
setAccumulator i = liftF (SetAccumulator i ())

logOutput :: String -> Computation ()
logOutput s = liftF (LogOutput s ())

-- The program
program :: Computation ()
program =
  replicateM_ 10 $ do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)

-- An interpreter
transform :: Operations a -> MonadTransformers.Computation a
transform (GetRandom k) = k <$> getRandomR (0, 9)
transform (GetAccumulator k) = gets k
transform (SetAccumulator i k) = k <$ put i
transform (LogOutput s k) = k <$ tell s

interpret :: Computation a -> (a, String)
interpret c = MonadTransformers.interpret (foldFree transform c)
