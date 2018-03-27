import BespokeMonadSpec
import MonadTransformersSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  BespokeMonadSpec.spec
  MonadTransformersSpec.spec
