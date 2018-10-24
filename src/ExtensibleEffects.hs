{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module ExtensibleEffects
  ( interpret
  , program
  ) where

{-
 - This example solves the challenge with the "extensible effects" framework
 - originally proposed in [1] and elaborated in [2].
 -
 - [1] Oleg Kiselyov, Amr Sabry, and Cameron Swords. 2013. Extensible effects:
 -     an alternative to monad transformers. In Proceedings of the 2013 ACM
 -     SIGPLAN symposium on Haskell (Haskell '13). ACM, New York, NY, USA,
 -     59-70. DOI=http://dx.doi.org/10.1145/2503778.2503791
 -
 - [2] Oleg Kiselyov and Hiromi Ishii. 2015. Freer monads, more extensible
 -     effects. In Proceedings of the 2015 ACM SIGPLAN Symposium on Haskell
 -     (Haskell '15). ACM, New York, NY, USA, 94-105.
 -     DOI=http://dx.doi.org/10.1145/2804302.2804319
 -}
import Control.Eff (Eff, Member, run)
import Control.Eff.Extend (handle_relay_s, send)
import Control.Eff.State.Lazy (State, get, put, runState)
import Control.Eff.Writer.Lazy (Writer, runMonoidWriter, tell)
import Control.Monad (replicateM_)
import Data.Typeable (Typeable)
import System.Random (mkStdGen, randomR)

-- A custom effect
data Random a where
  GetRandom :: Random Integer
  deriving (Typeable)

-- The operations
getRandom :: Member Random r => Eff r Integer
getRandom = send GetRandom

getAccumulator :: Member (State Integer) r => Eff r Integer
getAccumulator = get

setAccumulator :: Member (State Integer) r => Integer -> Eff r ()
setAccumulator = put

logOutput :: Member (Writer String) r => String -> Eff r ()
logOutput = tell

-- The program
program ::
     (Member Random r, Member (State Integer) r, Member (Writer String) r)
  => Eff r ()
program =
  replicateM_ 10 $ do
    i <- getAccumulator
    logOutput (show i ++ "\n")
    r <- getRandom
    setAccumulator (r + i)
    pure ()

-- A custom effect handler
runRandom :: Eff (Random ': r) a -> Eff r a
runRandom =
  handle_relay_s
    (mkStdGen 0)
    (const pure)
    (\s1 GetRandom k ->
       let (r, s2) = randomR (0, 9) s1
        in k s2 r)

-- An interpreter
interpret ::
     (forall r. ( Member Random r
                , Member (State Integer) r
                , Member (Writer String) r
                ) =>
                  Eff r a)
  -> (a, String)
interpret c =
  let ((x, o), _) =
        run . runState (0 :: Integer) . runMonoidWriter . runRandom $ c
   in (x, o)
