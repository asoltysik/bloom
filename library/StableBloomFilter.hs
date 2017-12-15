module StableBloomFilter(
  StableBloomFilter
  , newStableBloom
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits
import System.Random

import Prelude hiding (max)

import Bloom
import Internal.BitVector
import qualified Internal.Hashing as Hashing

data StableBloomFilter a = StableBloomFilter {
    cells :: Cells
  , m     :: Int
  , d     :: Int
  , p     :: Int
  , k     :: Int
  , max   :: Word64
  , rng   :: StdGen
} deriving (Show)

decrement :: StableBloomFilter a -> StableBloomFilter a
decrement bloom = 
  bloom { rng = newRng
        , cells = foldl (
          \a b -> 
          let value = getAtIndex b (d bloom) a - 1
              actualValue = if value < 0 then 0 else value
          in setAtIndex b (d bloom) actualValue a
        ) (cells bloom) indexes}
  where (index, newRng) = randomR (0, m bloom - p bloom) (rng bloom)
        indexes = [index..(index + p bloom - 1)]


instance BloomFilter StableBloomFilter where
  add item bloom = bloom { 
    cells = 
      foldl (\a b -> setAtIndex b (d bloom) (max bloom) a) (cells decremented) indexes 
    }
    where
      decremented = decrement bloom
      indexes = Hashing.listOfHashes item (m bloom) (k bloom)

  mightContain item bloom = 
    all (\i -> getAtIndex i (d bloom) (cells bloom) > 0) indexes
    where
      indexes = Hashing.listOfHashes item (m bloom) (k bloom)


optimalP :: Int -> Int -> Int -> Double -> Int
optimalP m k d fpRate = 
  let p = 1 / denom
  in if p <= 0 then 1 else truncate p
  where
    max = 2 ** (fromIntegral d) - 1
    subDenom = (1 - fpRate ** (1 / (fromIntegral k))) ** (1 / max)
    denom = (1 / subDenom - 1) * (1 / (fromIntegral k) - 1 / (fromIntegral m))
    
-- |`newStableBloom` creates a data structure for the Stable Bloom Algorithm
-- The arguments are:
-- size - number of cells in the data structure
-- cellSize - number of bits per cell
-- fpRate - desired maximum ratio of false positives (between 0 and 1)
newStableBloom :: Int -> Int -> Double -> StableBloomFilter a
newStableBloom size cellSize fpRate = StableBloomFilter { 
  cells = V.replicate actualSize 0 :: Cells
  , m = size
  , p = optimalP size actualK cellSize fpRate
  , k = actualK
  , d = cellSize
  , max = (shiftL 1 cellSize) - 1
  , rng = mkStdGen 1
  }
  where
    actualSize = ceiling $ (fromIntegral size) / 64.0
    optimalK = (ceiling $ logBase 2 (1 / fpRate)) `div` 2
    actualK = if optimalK > size then size else optimalK
