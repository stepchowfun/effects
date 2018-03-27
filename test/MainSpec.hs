import BigMonadSpec
import MonadTransformersSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  BigMonadSpec.spec
  MonadTransformersSpec.spec
