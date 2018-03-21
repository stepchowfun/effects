import GreetSpec (greetSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  greetSpec
