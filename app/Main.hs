module Main
  ( main
  ) where

import qualified BespokeMonad
import qualified ExtensibleEffects
import qualified FreeMonad
import qualified MonadTransformers

main :: IO ()
main = do
  putStrLn "Bespoke monad:\n"
  putStrLn . snd $ BespokeMonad.interpret BespokeMonad.program
  putStrLn "Extensible effects:\n"
  putStrLn . snd $ ExtensibleEffects.interpret ExtensibleEffects.program
  putStrLn "Free monad:\n"
  putStrLn . snd $ FreeMonad.interpret FreeMonad.program
  putStrLn "Monad transformers:\n"
  putStrLn . snd $ MonadTransformers.interpret MonadTransformers.program
