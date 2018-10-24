import qualified BespokeMonadSpec
import qualified ExtensibleEffectsSpec
import qualified FreeMonadSpec
import qualified MonadTransformersSpec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    BespokeMonadSpec.spec
    ExtensibleEffectsSpec.spec
    FreeMonadSpec.spec
    MonadTransformersSpec.spec
