{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.Function.ABI (ABI(..)) where

import Data.Data
import Data.Default
import Data.Hashable
import Foreign.C.Types

#include <ffi.h>

newtype ABI = ABI CInt deriving (Eq,Ord,Typeable)

instance Hashable ABI where
  hash (ABI n) = hash (fromIntegral n :: Int)

instance Default ABI where
  def = ABI (#const FFI_DEFAULT_ABI)

instance Bounded ABI where
  minBound = ABI (#const FFI_FIRST_ABI)
  maxBound = ABI (#const FFI_LAST_ABI)

instance Enum ABI where
  fromEnum (ABI n) = fromIntegral n
  toEnum n = ABI (fromIntegral n)
  succ (ABI n) | n == maxBound = error "ABI: succ maxBound"
               | otherwise     = ABI (succ n)
  pred (ABI n) | n == minBound = error "ABI: pred minBound"
               | otherwise     = ABI (pred n)
  enumFrom x       = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where bound | fromEnum y >= fromEnum x = maxBound
                | otherwise = minBound
