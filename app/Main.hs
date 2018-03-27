module Main (main) where

import qualified BespokeMonad
import qualified FreeMonad
import qualified MonadTransformers

main :: IO ()
main = do
  putStrLn "Bespoke monad:\n"
  BespokeMonad.ioProgram
  putStrLn "Monad transformers:\n"
  MonadTransformers.ioProgram
  putStrLn "Free monad:\n"
  FreeMonad.ioProgram
