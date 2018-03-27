module MonadTransformersSpec (spec) where

import Control.Monad.Random (mkStdGen, runRand)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import ExpectedOutput (expectedOutput)
import MonadTransformers (program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Monad transformers" $ do
  it "should produce the correct output" $ do
    let (((_, s), _), _) =
          runRand (runStateT (runWriterT program) 0) (mkStdGen 0)
    s `shouldBe` expectedOutput
