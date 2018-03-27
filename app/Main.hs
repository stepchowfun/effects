module Main (main) where

import BigMonad

main :: IO ()
main = do
  putStrLn "Big monad:\n"
  BigMonad.ioProgram
