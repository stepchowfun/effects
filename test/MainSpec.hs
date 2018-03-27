import Test.Hspec (hspec)
import qualified BespokeMonadSpec
import qualified FreeMonadSpec
import qualified MonadTransformersSpec

main :: IO ()
main = hspec $ do
  BespokeMonadSpec.spec
  FreeMonadSpec.spec
  MonadTransformersSpec.spec
