module FreeMonadSpec
  ( spec
  ) where

import ExpectedOutput (expectedOutput)
import FreeMonad (interpret, program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Free monad" $
  it "should produce the correct output" $
  snd (interpret program) `shouldBe` expectedOutput
