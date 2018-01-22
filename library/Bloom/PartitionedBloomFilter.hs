module Bloom.PartitionedBloomFilter (
    PartitionedBloomFilter
  , newPartitionedBloom
)
where --poprawic eksporty

import Data.Vector.Unboxed as V
import Internal.Hashing
import Internal.BitVector
import Internal.Util
import qualified Bloom as B



data PartitionedBloomFilter a = PartitionedBloomFilter {
    cells :: [Cells]
  , m :: Int    --rozmiar
  , k :: Int    --ilosc hashow
  , d :: Int    --rozmiar komorki
  , s :: Int    --rozmiar partycji
  , count :: Int--ilosc danych
} deriving (Show)


newPartitionedBloom ::  Int -> Double -> Int -> PartitionedBloomFilter a
newPartitionedBloom sizePerCell fpRate items = PartitionedBloomFilter {
    m = optimalSize
  , k = actualK
  , d = sizePerCell
  , s = ceiling $ fromIntegral $ optimalSize `div` actualK
  , cells = Prelude.replicate actualK (V.replicate (actualSize`div` optimalK) 0 :: Cells)
  , count = items
} where
    actualSize = ceiling $ (fromIntegral optimalSize) / 64.0
    actualK = if optimalK > optimalSize then optimalSize else optimalK
    optimalK = (ceiling $ logBase 2 (1 / fpRate)) `div` 2
    optimalSize = optimalM items 0.5 fpRate





