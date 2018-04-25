module BespokeMonadSpec
  ( spec
  ) where

import BespokeMonad (Computation(..), program)
import ExpectedOutput (expectedOutput)
import System.Random (mkStdGen)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Bespoke monad" $
  it "should produce the correct output" $ do
    let (_, _, s, _) = runComputation program (mkStdGen 0) 0
    s `shouldBe` expectedOutput
