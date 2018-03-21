import GreetSpec (greetSpec)
import Test.Hspec (hspec)

-- The QuickCheck specs

main :: IO ()
main = hspec $ do
  greetSpec
