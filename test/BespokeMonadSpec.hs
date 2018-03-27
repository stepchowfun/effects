module BespokeMonadSpec (spec) where

import BespokeMonad (Computation(..), program, run)
import ExpectedOutput (expectedOutput)
import System.Random (mkStdGen)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Bespoke monad" $ do
  it "should produce the correct output" $ do
    let (_, _, s, _) = run program (mkStdGen 0) 0
    s `shouldBe` expectedOutput
