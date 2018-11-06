{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}

module Modules where

{-

  Here we describe functionalities with simple datatypes, records of functions,
  and call them "modules"

  Then we define "constructors" those modules and specify how they depend on
  each other.

  Finally we put them in a "Registry" with the https://github.com/etorreborre/registry
  package and wire all of them into a top-level "App" running the whole program using
  all the "Modules"

-}
import           Data.IORef
import           Data.Registry
import           Protolude     as P hiding (get)
import           System.Random (getStdRandom, randomR)

-- | Top level application, created from the registry
app :: IO (App IO)
app = make @(IO (App IO)) registry

registry :: Registry
   -- inputs for constructors in the registry
  '[IO (Logger IO), IO (Random IO), IO (Accumulator IO)]
   -- outputs for constructors in the registry
   '[IO (Accumulator IO), IO (Logger IO), IO (Random IO), IO (App IO)]
registry =
     fun   newAccumulator
  +: funTo @IO newLogger
  +: funTo @IO newRandom
  +: funTo @IO newApp
  +: end

-- * Logging module, can go into its own library

newtype Logger m = Logger {
  info :: forall a . (Show a) => a -> m ()
}

newLogger :: Logger IO
newLogger = Logger P.print

-- * Random module, implemented using the global random generator
--   for simplicity

newtype Random m = Random {
  draw :: Int -> Int -> m Int
}

newRandom :: Random IO
newRandom =
  Random {
    draw = \l h -> getStdRandom (randomR (l, h))
  }

-- * Accumulator module
--   the constructor for this module is effectful
--   because we instantiate an IORef

data Accumulator m = Accumulator {
  add :: Int -> m ()
, get :: m Int
}

newAccumulator :: IO (Accumulator IO)
newAccumulator = do
  counter <- newIORef 0
  pure Accumulator {
    add = \n -> modifyIORef counter (+n)
  , get = readIORef counter
  }

-- * The top-level app containing the main program
--   It depends on other modules for its implementation

newtype App m = App  {
  run :: m ()
}

newApp :: Logger IO -> Random IO -> Accumulator IO -> App IO
newApp Logger{..} Random{..} Accumulator{..} = App {
  run = replicateM_ 10 $
          do current <- get
             _       <- info current
             picked  <- draw 0 9
             add picked
}
