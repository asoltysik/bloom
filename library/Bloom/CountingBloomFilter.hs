module Bloom.CountingBloomFilter(
  CountingBloomFilter
  , newCountingBloom
  
  , B.add
  , B.mightContain
  , B.remove
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Word

import Prelude hiding (max)

import qualified Bloom as B
import Internal.BitVector
import qualified Internal.Hashing as Hashing

data CountingBloomFilter a = CountingBloomFilter {
    cells :: Cells
  , m     :: Int
  , d     :: Int
  , k     :: Int
  , count :: Int
  , max   :: Word64
}

instance B.BloomFilter CountingBloomFilter where
  add item bloom = bloom {
    count = (count bloom) + 1,
    cells = foldl (
        \a b -> increment b (d bloom) (max bloom) 1 a
      ) (cells bloom) indexes
    }
    where 
      indexes = Hashing.listOfHashes item (m bloom) (k bloom)

  mightContain item bloom =
    all (\i -> getAtIndex i (d bloom) (cells bloom) > 0) indexes
    where
      indexes = Hashing.listOfHashes item (m bloom) (k bloom)

instance B.DeletableBloomFilter CountingBloomFilter where
  remove item bloom = bloom {
    count = (count bloom) - 1,
    cells = foldl (
        \a b -> increment b (d bloom) (max bloom) (-1) a
      ) (cells bloom) indexes
    }
    where
      indexes = Hashing.listOfHashes item (m bloom) (k bloom)
    

newCountingBloom :: Int -> Int -> Double -> CountingBloomFilter a
newCountingBloom size cellSize fpRate = CountingBloomFilter {
    cells = V.replicate actualSize 0 :: Cells
  , m = size
  , d = cellSize
  , k = actualK
  , count = 0
  , max = shiftL 1 cellSize - 1
  }
  where
    actualSize = ceiling $ (fromIntegral size) / 64.0
    optimalK = ceiling (logBase 2 (1 / fpRate)) `div` 2
    actualK = if optimalK > size then size else optimalK
