module BespokeMonadSpec (spec) where

import BespokeMonad (Computation(..), program, run)
import System.Random (mkStdGen)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Bespoke monad" $ do
  it "should produce the correct output" $ do
    let (_, _, s, _) = run program (mkStdGen 0) 0
    s `shouldBe` "0\n3\n6\n9\n17\n17\n24\n25\n26\n27\n"
