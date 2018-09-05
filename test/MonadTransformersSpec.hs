module MonadTransformersSpec
  ( spec
  ) where

import ExpectedOutput (expectedOutput)
import MonadTransformers (interpret, program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Monad transformers" $
  it "should produce the correct output" $
  snd (interpret program) `shouldBe` expectedOutput
