-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "haskell-bloom-streaming" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "should create stable bloom filter" $ do
        mightContain 13 (add 13 $ defaultStableBloom 10000 0.01) `shouldBe` True 
