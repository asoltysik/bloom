-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Bloom.StableBloomFilter
import Data.BloomFilter

adding :: [Int] -> StableBloomFilter Int
adding list = 
  foldl (\a b -> add b a) bloom list
  where bloom = newStableBloom 100000 1 0.03

checking :: [Int] -> StableBloomFilter Int -> [Bool]
checking list bloom =
  map (\elem -> mightContain elem bloom) list
  
main :: IO ()
main = defaultMain [
  bgroup "adding" 
    [ bench "1000" $ whnf adding [0..1000]
    , bench "10000" $ whnf adding [0..10000]
    , bench "100000" $ whnf adding [0..100000]
    , bench "1000000" $ whnf adding [0..1000000]
    ]
  , bgroup "checking"
    [ bench "1000" $ nf (checking [0..1000]) (adding [0..1000])
    , bench "10000" $ nf (checking [0..10000]) (adding [0..10000])
    , bench "100000" $ nf (checking [0..100000]) (adding [0..100000])
    , bench "1000000" $ nf (checking [0..1000000]) (adding [0..1000000])
    ]
  ]
