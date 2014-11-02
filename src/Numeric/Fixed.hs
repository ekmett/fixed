{-# LANGUAGE CApiFFI, DeriveDataTypeable #-}
module Numeric.Fixed 
  ( Fixed(..)
  ) where

import Data.Bits
import Data.Coerce
import Data.Int
import Data.Ratio
import Data.Typeable

-- | A signed 2s complement 15.16 scale fixed precision number
newtype {-# CTYPE "signed int" #-} Fixed = Fixed { getFixed :: Int32 } deriving (Eq,Ord,Typeable)

toFloat :: Fixed -> Float
toFloat (Fixed x) = fromIntegral x / 65536

toFixed :: Float -> Fixed
toFixed x = Fixed $ floor (x * 65536 + 0.5)

instance Show Fixed where
  showsPrec d = showsPrec d . toFloat

instance Num Fixed where
  (+) = coerce ((+) :: Int32 -> Int32 -> Int32)
  (-) = coerce ((-) :: Int32 -> Int32 -> Int32)
  negate = coerce (negate :: Int32 -> Int32)
  abs = coerce (abs :: Int32 -> Int32)
  signum = coerce (signum :: Int32 -> Int32)
  Fixed a * Fixed b = Fixed $ fromIntegral (unsafeShiftR (fromIntegral a * fromIntegral b) 16 :: Int64)
  fromInteger i = Fixed $ unsafeShiftL (fromInteger i) 16

instance Fractional Fixed where
  Fixed a / Fixed b  = Fixed $ fromIntegral (unsafeShiftL (fromIntegral a) 16 `div` fromIntegral b :: Int64)
  fromRational a = Fixed $ fromInteger (unsafeShiftL (numerator a) 16 `div` denominator a)

instance Real Fixed where
  toRational (Fixed i) = toInteger i % 65536

instance RealFrac Fixed where
  properFraction (Fixed a) 
    | a >= 0 = (fromIntegral (unsafeShiftR a 16), Fixed (a .&. 0xffff))
    | otherwise = (negate $ fromIntegral $ unsafeShiftR (negate a) 16, Fixed $ (a .&. 0xffff) - 0x10000)
  truncate (Fixed a) 
    | a >= 0 = fromIntegral (unsafeShiftR a 16)
    | otherwise = negate $ fromIntegral $ unsafeShiftR (negate a) 16
  round (Fixed f)   = fromIntegral $ unsafeShiftR (f + 0x800) 16 
  ceiling (Fixed f) = fromIntegral $ unsafeShiftR (f + 0xffff) 16
  floor (Fixed f)   = fromIntegral $ unsafeShiftR f 16
