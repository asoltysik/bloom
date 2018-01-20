module Bloom (
  BloomFilter
  , add
  , mightContain

  , DeletableBloomFilter
  , remove
  ) where

import Data.Serialize

class BloomFilter f where
  add :: (Serialize a) => a -> f a -> f a
  mightContain :: (Serialize a) => a -> f a -> Bool

class (BloomFilter f) => DeletableBloomFilter f where
  remove :: (Serialize a) => a -> f a -> f a
