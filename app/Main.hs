module Main (main) where

import BigMonad
import MonadTransformers

main :: IO ()
main = do
  putStrLn "Big monad:\n"
  BigMonad.ioProgram
  putStrLn "Monad transformers:\n"
  MonadTransformers.ioProgram
