{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Modules (
  app
, run
) where

{-

  This module describes a different approach to handling effects based on the
  the idea that the idea of "having to manage effects" is primarily about
  being able to compose functionality through the use of some interfaces
  for example:

   - the functions of the StateT monad transformer in MonadTransformers
   - the "state" effect in ExtensibleEffects

  Then we wish to "assemble" the implementations with:

   - monad transformers which compose so that `StateT m` is still a
   monad if m is a Monad

   - interpreters for extensible effects

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
  '[IO LogModule, IO RandomModule, IO AccumulatorModule]
   -- outputs for constructors in the registry
   '[IO AccumulatorModule, IO LogModule, IO RandomModule, IO App]
registry =
     fun   newAccumulatorModule
  +: funTo @IO newLogModule
  +: funTo @IO newRandomModule
  +: funTo @IO newApp
  +: end

-- * Logging module, can go into its own library

newtype LogModule = LogModule {
  info :: forall a . (Show a) => a -> IO ()
}

newLogModule :: LogModule
newLogModule = LogModule P.print

-- * Random module, implemented using the global random generator
--   for simplicity

newtype RandomModule = RandomModule {
  draw :: Int -> Int -> IO Int
}

newRandomModule :: RandomModule
newRandomModule =
  RandomModule {
    draw = \l h -> getStdRandom (randomR (l, h))
  }

-- * Accumulator module
--   the constructor for this module is effectful
--   because we instantiate an IORef

data AccumulatorModule = AccumulatorModule {
  add :: Int -> IO ()
, get :: IO Int
}

newAccumulatorModule :: IO AccumulatorModule
newAccumulatorModule = do
  counter <- newIORef 0
  pure $ AccumulatorModule {
    add = \n -> modifyIORef counter (+n)
  , get = readIORef counter
  }

-- * The top-level app containing the main program
--   It depends on other modules for its implementation

newtype App = App  {
  run :: IO ()
}

newApp :: LogModule -> RandomModule -> AccumulatorModule -> App
newApp logging random accumulator = App {
  run = replicateM_ 10 $
          do current <- accumulator & get
             _       <- (logging & info) $ current
             picked  <- (random & draw) 0 9
             accumulator & add $ picked
}
