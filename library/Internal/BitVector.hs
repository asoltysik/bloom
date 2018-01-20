module Internal.BitVector
  where

import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits

type Cells = V.Vector Word64

setAtIndex :: Int -> Int -> Word64 -> Cells -> Cells
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


getAtIndex :: Int -> Int -> Cells -> Word64
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

increment :: Int -> Int -> Word64 -> Word64 -> Cells -> Cells
increment index len maxVal amount cells =
  setAtIndex index len actualValue cells
  where
    value = getAtIndex index len cells + amount
    actualValue =
      if value < 0 then 0
      else 
        if value > maxVal then maxVal
        else value
