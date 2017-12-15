module Bloom (
  BloomFilter
  , add
  , mightContain
  ) where

import Data.Serialize

class BloomFilter f where
  add :: (Serialize a) => a -> f a -> f a
  mightContain :: (Serialize a) => a -> f a -> Bool

