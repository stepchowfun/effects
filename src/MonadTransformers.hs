module MonadTransformers
  ( Computation
  , interpret
  , ioProgram
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
import Control.Monad.Random (Rand, StdGen, getRandomR, mkStdGen, runRand)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)

-- The monad
type Computation = WriterT String (StateT Integer (Rand StdGen))

-- The operations
getRandom :: Computation Integer
getRandom = getRandomR (0, 9)

getAccumulator :: Computation Integer
getAccumulator = get

setAccumulator :: Integer -> Computation ()
setAccumulator = put

logOutput :: String -> Computation ()
logOutput = tell

-- The program
program :: Computation ()
program =
  replicateM_ 10 $ do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)
    pure ()

-- An interpreter
interpret :: Computation a -> IO a
interpret c =
  let (((x, s), _), _) = runRand (runStateT (runWriterT c) 0) (mkStdGen 0)
  in putStrLn s >> pure x

-- An interpretation of the program
ioProgram :: IO ()
ioProgram = interpret program
