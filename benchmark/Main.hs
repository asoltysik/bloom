-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import StableBloomFilter
import Data.BloomFilter

adding :: Int -> StableBloomFilter Int
adding n = 
  foldl (\a b -> add b a) bloom [0..n]
  where bloom = defaultStableBloom 100000 1 0.01
  
main :: IO ()
main = defaultMain [
  bgroup "adding" [ bench "100" $ whnf adding 100
                  , bench "1000" $ whnf adding 1000
                  , bench "10000" $ whnf adding 10000
                  , bench "100000" $ whnf adding 100000
                  , bench "1000000" $ whnf adding 1000000
                  ]
  ]
