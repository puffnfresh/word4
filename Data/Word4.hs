{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Word4 (
  Word4
, toWord8
, splitWord8
, joinWord4
) where

import           Data.Array (Ix (..))
import           Data.Bits  (Bits (..), FiniteBits (..))
import           Data.Word  (Word8)

newtype Word4
  = Word4 Word8
  deriving (Eq, Ord, Num)

toWord8 :: Word4 -> Word8
toWord8 (Word4 a) =
  a .&. 0xF

splitWord8 :: Word8 -> (Word4, Word4)
splitWord8 w =
  (Word4 $ w `shiftR` 4, Word4 w)

joinWord4 :: (Word4, Word4) -> Word8
joinWord4 (a, b) =
  toWord8 a `shiftL` 4 .|. toWord8 b

instance Show Word4 where
  show =
    show . toWord8

instance Bounded Word4 where
  minBound =
    Word4 0
  maxBound =
    Word4 0xF

instance Bits Word4 where
  a .&. b =
    Word4 $ toWord8 a .&. toWord8 b
  a .|. b =
    Word4 $ toWord8 a .|. toWord8 b
  xor a b =
    Word4 $ xor (toWord8 a) (toWord8 b)
  complement =
    Word4 . complement . toWord8
  shiftL a i =
    Word4 $ shiftL (toWord8 a) i
  shiftR a i =
    Word4 $ shiftR (toWord8 a) i
  rotate a i =
    Word4 $ rotate (toWord8 a) i
  bitSize _ =
    4
  bitSizeMaybe _ =
    Just 4
  isSigned _ =
    False
  testBit =
    testBit . toWord8
  bit =
    Word4 . bit
  popCount =
    popCount . toWord8

instance FiniteBits Word4 where
  finiteBitSize _ =
    4

instance Ix Word4 where
  range (a, b) =
    Word4 <$> [toWord8 a..toWord8 b]
  index (a, _) i =
    fromIntegral $ toWord8 (i - a)
  inRange (a, b) i =
    a <= i && i <= b
