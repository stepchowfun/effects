module MonadTransformersSpec (spec) where

import Control.Monad.Random (mkStdGen, runRand)
import Control.Monad.State (runStateT)
import Control.Monad.Writer (runWriterT)
import MonadTransformers (program)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Monad transformers" $ do
  it "should produce the correct output" $ do
    let (((_, s), _), _) =
          runRand (runStateT (runWriterT program) 0) (mkStdGen 0)
    s `shouldBe` "0\n3\n6\n9\n17\n17\n24\n25\n26\n27\n"
