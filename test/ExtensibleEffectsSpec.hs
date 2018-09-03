module ExtensibleEffectsSpec
  ( spec
  ) where

import Control.Eff (run)
import Control.Eff.State.Lazy (runState)
import Control.Eff.Writer.Lazy (runMonoidWriter)
import ExpectedOutput (expectedOutput)
import ExtensibleEffects (program, runRandom)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "Extensible effects" $
  it "should produce the correct output" $ do
    let ((_, output), _) =
          run . runState (0 :: Integer) . runMonoidWriter . runRandom $ program
    output `shouldBe` expectedOutput
