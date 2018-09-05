module ExtensibleEffectsSpec
  ( spec
  ) where

import ExpectedOutput (expectedOutput)
import ExtensibleEffects (interpret, program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Extensible effects" $
  it "should produce the correct output" $
  snd (interpret program) `shouldBe` expectedOutput
