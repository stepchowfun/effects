module FreeMonadSpec
  ( spec
  ) where

import Control.Monad.Random (mkStdGen, runRand)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import ExpectedOutput (expectedOutput)
import FreeMonad (interpret, program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Free monad" $
  it "should produce the correct output" $ do
    let (((_, s), _), _) =
          runRand (runStateT (runWriterT (interpret program)) 0) (mkStdGen 0)
    s `shouldBe` expectedOutput
