module Bloom.PartitionedBloomFilter (
    PartitionedBloomFilter
  , newPartitionedBloom
)
where --poprawic eksporty

import Data.Vector.Unboxed as V
import Internal.Hashing
import Internal.BitVector
import Internal.Util

data PartitionedBloomFilter a = PartitionedBloomFilter {
    cells :: [Cells]
  , m :: Int    --rozmiar
  , k :: Int    --ilosc hashow
  , d :: Int    --rozmiar komorki
  , s :: Int    --rozmiar partycji
  , count :: Int--ilosc danych
} deriving (Show)


newPartitionedBloom :: Int -> Int -> Double -> Int -> PartitionedBloomFilter a
newPartitionedBloom size sizePerCell fpRate items = PartitionedBloomFilter {
    m = optimalM items 0.5 fpRate
  , k = actualK
  , d = sizePerCell
  , s = ceiling $ fromIntegral $ size `div` actualK
  , cells = Prelude.replicate actualK (V.replicate actualSize 0 :: Cells)
  , count = items
} where
    actualSize = ceiling $ (fromIntegral size) / 64.0
    actualK = if optimalK > size then size else optimalK
    optimalK = (ceiling $ logBase 2 (1 / fpRate)) `div` 2





