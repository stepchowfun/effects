{-# LANGUAGE FlexibleContexts #-}

module MonadTransformers
  ( Computation
  , interpret
  , program
  ) where

{-
 - This example solves the challenge in the most standard way: with a monad
 - transformer stack. The idea of monad transformers came from [1].
 -
 - [1] Sheng Liang, Paul Hudak, and Mark Jones. 1995. Monad transformers and
 -     modular interpreters. In Proceedings of the 22nd ACM SIGPLAN-SIGACT
 -     symposium on Principles of programming languages (POPL '95). ACM, New
 -     York, NY, USA, 333-343. DOI=http://dx.doi.org/10.1145/199448.199528
 -}
import Control.Monad (replicateM_)
import Control.Monad.Random
  ( MonadRandom
  , Rand
  , StdGen
  , getRandomR
  , mkStdGen
  , runRand
  )
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Control.Monad.Writer (MonadWriter, WriterT, runWriterT, tell)

-- The monad
type Computation = WriterT String (StateT Integer (Rand StdGen))

-- The operations
getRandom :: MonadRandom m => m Integer
getRandom = getRandomR (0, 9)

getAccumulator :: MonadState Integer m => m Integer
getAccumulator = get

setAccumulator :: MonadState Integer m => Integer -> m ()
setAccumulator = put

logOutput :: MonadWriter String m => String -> m ()
logOutput = tell

-- The program
program :: (MonadRandom m, MonadState Integer m, MonadWriter String m) => m ()
program =
  replicateM_ 10 $ do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)
    pure ()

-- An interpreter
interpret :: Computation a -> (a, String)
interpret c =
  let ((x, _), _) = runRand (runStateT (runWriterT c) 0) (mkStdGen 0)
  in x
