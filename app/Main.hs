module Main (main) where

import BespokeMonad
import MonadTransformers

main :: IO ()
main = do
  putStrLn "Bespoke monad:\n"
  BespokeMonad.ioProgram
  putStrLn "Monad transformers:\n"
  MonadTransformers.ioProgram
