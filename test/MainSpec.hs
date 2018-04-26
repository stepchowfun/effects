import qualified BespokeMonadSpec
import qualified FreeMonadSpec
import qualified MonadTransformersSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    BespokeMonadSpec.spec
    FreeMonadSpec.spec
    MonadTransformersSpec.spec
