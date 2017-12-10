module StableBloomFilter(
  -- BloomFilter
  BloomFilter
  , add
  , mightContain

  -- StableBloomFilter
  , StableBloomFilter
  , defaultStableBloom
  ) where

import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits
import Data.Dish.Murmur3
import Data.Serialize

import Prelude hiding (max)

class BloomFilter f where
  add :: (Serialize a) => a -> f a -> f a
  mightContain :: (Serialize a) => a -> f a -> Bool

data StableBloomFilter a = StableBloomFilter {
    cells :: V.Vector Word64
  , m     :: Int
  , d     :: Int
  , p     :: Int
  , k     :: Int
  , max   :: Word64
} deriving (Show)

setAtIndex :: Int -> Int -> Word64 -> V.Vector Word64 -> V.Vector Word64
setAtIndex index len bits cells = 
  if wordOffset + len > 64 then
    let rem = 64 - wordOffset
        firstPart = setAtIndex index (rem) bits cells
    in 
      setAtIndex (wordOffset + rem) (len - rem) (shiftR bits rem) firstPart 
  else
    let bitMask = (shiftL 1 len) - 1
        firstValue = (cells V.! wordIndex) .&. (complement $ shiftL bitMask wordOffset)
        secondValue = firstValue .|. shiftL (bits .&. bitMask) wordOffset
    in 
      V.update cells $ V.singleton (wordIndex, secondValue)
  where wordIndex = index `div` 64
        wordOffset = index `mod` 64


getAtIndex :: Int -> Int -> V.Vector Word64 -> Word64
getAtIndex index len cells = 
  if wordOffset + len > 64 then
    let rem = 64 - wordOffset
        firstPart = getAtIndex index rem cells
        secondPart = shiftL (getAtIndex (index + rem) (len - rem) cells) rem
    in firstPart .|. secondPart
  else
    let bitMask = (shiftL 1 len) - 1
        firstPart = cells V.! wordIndex
        secondPart = shiftL bitMask wordOffset
    in shiftR (firstPart .&. secondPart) wordOffset
  where
    wordIndex = index `div` 64
    wordOffset = index `mod` 64

decrement :: StableBloomFilter a -> StableBloomFilter a


instance BloomFilter StableBloomFilter where
  add item bloom = bloom { 
    cells = 
      foldl (\a b -> setAtIndex b (d bloom) (max bloom) a) decrementedCells indexes 
    }
    where
      decrementedCells = decrement bloom
      hash = murmur3IntegerX64 (encode item) 1337
      lower = hash .&. 0xFFFFFFFFFFFFFFFF
      upper = hash `shiftR` 64
      indexes = [fromIntegral $ 
                (lower + upper * (toInteger i)) `mod` (toInteger $ m bloom) 
                | i <- [0..(k bloom - 1)]]

  mightContain item bloom = 
    all (\i -> getAtIndex i (d bloom) (cells bloom) > 0) indexes
    where
      hash = murmur3IntegerX64 (encode item) 1337
      lower = hash .&. 0xFFFFFFFFFFFFFFFF
      upper = hash `shiftR` 64
      indexes = [fromIntegral $ 
                (lower + upper * (toInteger i)) `mod` (toInteger $ m bloom) 
                | i <- [0..(k bloom - 1)]]

optimalP :: Int -> Int -> Int -> Double -> Int
optimalP m k d fpRate = 
  let p = 1 / denom
  in if p <= 0 then 1 else truncate p
  where
    max = 2 ** (fromIntegral d) - 1
    subDenom = (1 - fpRate ** (1 / (fromIntegral k))) ** (1 / max)
    denom = (1 / subDenom - 1) * (1 / (fromIntegral k) - 1 / (fromIntegral m))
    

defaultStableBloom :: Int -> Int -> Double -> StableBloomFilter a
defaultStableBloom size d fpRate = StableBloomFilter { 
  cells = V.replicate actualSize 0 :: V.Vector Word64
  , m = size
  , p = optimalP size actualK d fpRate
  , k = actualK
  , d = d
  , max = (shiftL 1 d) - 1 
  }
  where
    actualSize = ceiling $ (fromIntegral size) / 64.0
    optimalK = (ceiling $ logBase 2 (1 / fpRate)) `div` 2
    actualK = if optimalK > size then size else optimalK
