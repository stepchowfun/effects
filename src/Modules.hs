{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Modules (
  app
, run
) where

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
app :: IO App
app = make @(IO App) registry

registry :: Registry
   -- inputs for constructors in the registry
  '[IO Logger, IO Random, IO Accumulator]
   -- outputs for constructors in the registry
   '[IO Accumulator, IO Logger, IO Random, IO App]
registry =
     fun   newAccumulator
  +: funTo @IO newLogger
  +: funTo @IO newRandom
  +: funTo @IO newApp
  +: end

-- * Logging module, can go into its own library

newtype Logger = Logger {
  info :: forall a . (Show a) => a -> IO ()
}

newLogger :: Logger
newLogger = Logger P.print

-- * Random module, implemented using the global random generator
--   for simplicity

newtype Random = Random {
  draw :: Int -> Int -> IO Int
}

newRandom :: Random
newRandom =
  Random {
    draw = \l h -> getStdRandom (randomR (l, h))
  }

-- * Accumulator module
--   the constructor for this module is effectful
--   because we instantiate an IORef

data Accumulator = Accumulator {
  add :: Int -> IO ()
, get :: IO Int
}

newAccumulator :: IO Accumulator
newAccumulator = do
  counter <- newIORef 0
  pure $ Accumulator {
    add = \n -> modifyIORef counter (+n)
  , get = readIORef counter
  }

-- * The top-level app containing the main program
--   It depends on other modules for its implementation

newtype App = App  {
  run :: IO ()
}

newApp :: Logger -> Random -> Accumulator -> App
newApp logger random accumulator = App {
  run = replicateM_ 10 $
          do current <- accumulator & get
             _       <- (logger & info) $ current
             picked  <- (random & draw) 0 9
             accumulator & add $ picked
}

