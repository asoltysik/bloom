module Internal.Hashing
  where

import Data.Serialize
import Data.Dish.Murmur3
import Data.Bits

listOfHashes :: (Serialize a) => a -> Int -> Int -> [Int]
listOfHashes item max numHashes = 
  [fromIntegral $ (lower + upper * (toInteger i)) `mod` (toInteger max)
  | i <- [0..(numHashes - 1)]]
  where hash = murmur3IntegerX64 (encode item) 1337
        lower = hash .&. 0xFFFFFFFFFFFFFFFF
        upper = hash `shiftR` 64
