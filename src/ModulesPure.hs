{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-
  Pure instantiation of the application
-}
module ModulesPure where

import           Control.Monad.Random (Rand, StdGen, getRandomR, mkStdGen,
                                       runRand)
import           Control.Monad.State  as State (MonadState, StateT, runStateT, get, modify)
import           Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)
import           Data.Registry
import           Modules
import           Protolude            as P hiding (get)
import           System.Random        (getStdRandom, randomR)

-- Pure interface for the components
type P = WriterT String (StateT Int (Rand StdGen))

-- | Top level application, created from the registry
appPure :: App P
appPure = make @(App P) registryPure

registryPure :: Registry
   -- inputs for constructors in the registry
  '[Logger P, Random P, Accumulator P]
   -- outputs for constructors in the registry
  '[Accumulator P, Logger P, Random P, App P]
registryPure =
     fun newAccumulatorPure
  +: fun newLoggerPure
  +: fun newRandomPure
  +: fun newAppPure
  +: end

newLoggerPure :: Logger P
newLoggerPure = Logger (tell . P.show)

newRandomPure :: Random P
newRandomPure =
  Random {
    draw = \l h -> getRandomR (l, h)
  }

newAccumulatorPure :: Accumulator P
newAccumulatorPure =
  Accumulator {
    add = \n -> State.modify (+n)
  , get = State.get
  }

newAppPure :: Logger P -> Random P -> Accumulator P -> App P
newAppPure Logger{..} Random{..} Accumulator{..} = App {
  run = replicateM_ 10 $
          do current <- get
             _       <- info current
             picked  <- draw 0 9
             add picked
}
