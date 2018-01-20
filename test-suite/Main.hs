-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Bloom.StableBloomFilter

main :: IO ()
main = do
    test <- testSpec "bloom" spec
    Test.Tasty.defaultMain test

values = [1, 2, 3, 45, 100, 220, 25232] :: [Int]

spec :: Spec
spec = parallel $ do
    it "should work for a few elements" $ do
      let bloom1 = newStableBloom 10000 2 0.05
      let bloom2 = foldl (\b elem -> add elem b) bloom1 values
      all (\elem -> mightContain elem bloom2) values `shouldBe` True 
