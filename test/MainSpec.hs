import BigMonadSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  BigMonadSpec.spec
