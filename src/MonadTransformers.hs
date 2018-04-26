module MonadTransformers
  ( Computation
  , run
  , ioProgram
  , program
  ) where

{-
 - This example solves the challenge in the most standard way: with a monad
 - transformer stack.
 -}
import Control.Monad (replicateM_)
import Control.Monad.Random (Rand, StdGen, getRandomR, mkStdGen, runRand)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)

type Computation = WriterT String (StateT Integer (Rand StdGen))

run :: Computation a -> IO a
run c =
  let (((x, s), _), _) = runRand (runStateT (runWriterT c) 0) (mkStdGen 0)
  in putStrLn s >> return x

program :: Computation ()
program =
  replicateM_ 10 $ do
    i <- get
    tell (show i ++ "\n")
    r <- getRandomR (0, 9)
    put (r + i)
    return ()

ioProgram :: IO ()
ioProgram = run program
