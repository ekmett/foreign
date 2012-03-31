{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Foreign.Function.Interface.Type
  (
  -- * Types for FFI
    Type(..)
  -- * common types
  , void, sint8, uint8, uint16, sint16, uint32, sint32, uint64, sint64, float, double, pointer
  -- * types that vary by platform
  , int, long, size_t, ptrdiff_t
  -- * tools for figuring out or constructing other FFI types
  , struct
  , signed, unsigned, floating
  ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.StablePtr
import Data.Typeable
import System.IO.Unsafe
import Data.IORef

#include <ffi.h>

data CType deriving Typeable

newtype Type = Type { foreignType :: ForeignPtr CType } deriving Typeable

withType :: Type -> (Ptr CType -> IO a) -> IO a
withType (Type fp) = withForeignPtr fp

-- types for invoking foreign function with structures
struct :: [Type] -> Type
struct xs = unsafeDupablePerformIO $ do
  let n = length xs
  es <- mallocBytes $ sizeOf (undefined :: Ptr CType) * (n + 1)
  sxs <- newStablePtr xs
  let go [] i = pokeElemOff es i nullPtr
      go es (Type fp:ts) i = do
        pokeElemOff es i (unsafeForeignPtrToPtr fp)
        go ts $! i + 1
  -- the stable ptr will keep all the foreign pointers alive, so we can just use unsafeForeignPtrToPtr
  -- but more importantly, this will keep the types we depend on from being freed before we are!
  go es xs 0
  r <- mallocForeignPtrBytes (#size ffi_type)
  (#poke ffi_type, size) r 0
  (#poke ffi_type, alignment) r 0
  (#poke ffi_type, type) r 0
  (#poke ffi_type, elements) r es
  addForeignPtrHaskellFinalizer r $ do
    free elements
    freeStablePtr sxs
  return r

intrinsic :: Ptr CType -> Type
intrinsic pt = unsafeDupablePerformIO $ newForeignPtr_ pt
{-# INLINE intrinsic #-}

foreign import ccall unsafe "&" ffi_type_void    :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint8   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint8   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint16  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint16  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint32  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint32  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint64  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint64  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_float   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_double  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_pointer :: Ptr CType

void, sint8, uint8, uint16, sint16, uint32, uint64, float, double, pointer :: Type
void    = intrinsic ffi_type_void
{-# NOINLINE void #-}
sint8   = intrinsic ffi_type_sint8
{-# NOINLINE sint8 #-}
uint8   = intrinsic ffi_type_uint8
{-# NOINLINE uint8 #-}
uint16  = intrinsic ffi_type_uint16
{-# NOINLINE uint16 #-}
sint16  = intrinsic ffi_type_sint16
{-# NOINLINE sint16 #-}
uint32  = intrinsic ffi_type_uint32
{-# NOINLINE uint32 #-}
sint32  = intrinsic ffi_type_sint32
{-# NOINLINE sint32 #-}
uint64  = intrinsic ffi_type_uint64
{-# NOINLINE uint64 #-}
sint64  = intrinsic ffi_type_sint64
{-# NOINLINE sint64 #-}
float   = intrinsic ffi_type_float
{-# NOINLINE float #-}
double  = intrinsic ffi_type_double
{-# NOINLINE double #-}
pointer = intrinsic ffi_type_pointer
{-# NOINLINE pointer #-}

int, long, size_t, ptrdiff_t :: Type
int = signed (undefined :: Int)
{-# NOINLINE int #-}
long = signed (undefined :: Long)
{-# NOINLINE long #-}
size_t = unsigned (undefined :: CSize)
{-# NOINLINE size_t #-}
ptrdiff_t = signed (undefined CPtrdiff)
{-# NOINLINE ptrdiff_t #-}

signed :: Storable a => a -> Type
signed proxy = case sizeOf proxy of
  1 -> sint8
  2 -> sint16
  4 -> sint32
  8 -> sint64
{-# INLINE CONLIKE signed #-}

unsigned :: Storable a => a -> Type
unsigned proxy = case sizeOf proxy of
  1 -> uint8
  2 -> uint16
  4 -> uint32
  8 -> uint64
{-# INLINE CONLIKE unsigned #-}

floating :: Storable a => a -> Type
floating proxy = case sizeOf proxy of
  4 -> float
  8 -> double
{-# INLINE CONLIKE floating #-}
