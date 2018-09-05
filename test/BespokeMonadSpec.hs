module BespokeMonadSpec
  ( spec
  ) where

import BespokeMonad (interpret, program)
import ExpectedOutput (expectedOutput)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Bespoke monad" $
  it "should produce the correct output" $
  snd (interpret program) `shouldBe` expectedOutput
