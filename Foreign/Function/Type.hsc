{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Foreign.Function.Type
  (
  -- * Types for FFI
    Type(..)
  -- * simple
  , void
  , float, double
  , sint8, uint8
  , uint16, sint16
  , uint32, sint32
  , uint64, sint64
  , pointer
  -- * complex
  , struct
  -- * helpers
  , signed, unsigned, floating
  ) where

import Control.Monad hiding (void)
-- import Control.Monad.Fix (mfix)
import Data.Data
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable
import Data.Interned
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Text.Read

#include <ffi.h>

foreign import ccall unsafe "&" ffi_type_void    :: Ptr CType
foreign import ccall unsafe "&" ffi_type_float   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_double  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint8   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint8   :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint16  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint16  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint32  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint32  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_uint64  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_sint64  :: Ptr CType
foreign import ccall unsafe "&" ffi_type_pointer :: Ptr CType

data CType deriving Typeable

data Type = Type
  { typeId    :: {-# UNPACK #-} !Id
  , typePtr   :: {-# UNPACK #-} !(Ptr CType)
  , typeParts :: Either String [Type]
  } deriving (Data,Typeable)

instance Eq Type where
  a == b = typeId a == typeId b

instance Hashable Type where
  hash = typeId
  hashWithSalt n t = hashWithSalt n (typeId t)

instance Show Type where
  showsPrec _ (Type _ _ (Left r)) = showString r
  showsPrec d (Type _ _ (Right xs)) = showParen (d > 10) $
    showString "struct " . showsPrec 10 xs

intrinsics :: HashMap String Type
intrinsics = HashMap.fromList
  [ ("void",    void)
  , ("float",   float)
  , ("double",  double)
  , ("sint8",   sint8)
  , ("uint8",   uint8)
  , ("sint16",  sint16)
  , ("uint16",  uint16)
  , ("sint32",  sint32)
  , ("uint32",  uint32)
  , ("sint64",  sint64)
  , ("uint64",  uint64)
  , ("pointer", pointer)
  ]

instance Read Type where
  readPrec = parens $ do
    Ident s <- lexP
    if s == "struct" then liftM struct readPrec
                     else maybe pfail return $ HashMap.lookup s intrinsics

struct :: [Type] -> Type
struct = intern

mallocArray :: Storable a => Int -> IO (Ptr a)
mallocArray n = r where
  r = mallocBytes $ sizeOf (argArg r) * n

argArg :: IO (Ptr a) -> a
argArg = undefined

instance Interned Type where
  newtype Description Type = Struct [Id] deriving Eq
  type Uninterned Type = [Type]
  describe xs = Struct (map typeId xs)
  identify slot xs = unsafePerformIO $ do
    let n = length xs
    es <- mallocArray (n + 1)
    let go [] i = pokeElemOff es i nullPtr
        go (t:ts) i = do
          pokeElemOff es i (typePtr t)
          go ts $! i + 1
    go xs 0
    r <- mallocBytes (#size ffi_type)
    (#poke ffi_type, size)      r (0 :: CSize)
    (#poke ffi_type, alignment) r (0 :: CShort)
    (#poke ffi_type, type)      r (0 :: CShort)
    (#poke ffi_type, elements) r es
    return $ Type slot r (Right xs)
  identity = typeId
  seedIdentity _ = 1 + (#const FFI_TYPE_LAST)
  cacheWidth _ = 128
  cache = typeCache

typeCache :: Cache Type
typeCache = mkCache
{-# NOINLINE typeCache #-}

instance Hashable (Description Type) where
  hashWithSalt n (Struct xs) = hashWithSalt n xs
  hash (Struct xs) = hash xs

void, float, double, sint8, uint8, uint16, sint16, uint32, sint32, uint64, sint64, pointer :: Type
void    = Type (#const FFI_TYPE_VOID)    ffi_type_void    (Left "void")
float   = Type (#const FFI_TYPE_FLOAT)   ffi_type_float   (Left "float")
double  = Type (#const FFI_TYPE_DOUBLE)  ffi_type_double  (Left "double")
sint8   = Type (#const FFI_TYPE_SINT8)   ffi_type_sint8   (Left "sint8")
uint8   = Type (#const FFI_TYPE_UINT8)   ffi_type_uint8   (Left "uint8")
uint16  = Type (#const FFI_TYPE_UINT16)  ffi_type_uint16  (Left "uint16")
sint16  = Type (#const FFI_TYPE_SINT16)  ffi_type_sint16  (Left "sint16")
uint32  = Type (#const FFI_TYPE_UINT32)  ffi_type_uint32  (Left "uint32")
sint32  = Type (#const FFI_TYPE_SINT32)  ffi_type_sint32  (Left "sint32")
uint64  = Type (#const FFI_TYPE_UINT64)  ffi_type_uint64  (Left "uint64")
sint64  = Type (#const FFI_TYPE_SINT64)  ffi_type_sint64  (Left "sint64")
pointer = Type (#const FFI_TYPE_POINTER) ffi_type_pointer (Left "pointer")
{-# NOINLINE void #-}
{-# NOINLINE float #-}
{-# NOINLINE double #-}
{-# NOINLINE uint8 #-}
{-# NOINLINE sint8 #-}
{-# NOINLINE uint16 #-}
{-# NOINLINE sint16 #-}
{-# NOINLINE uint32 #-}
{-# NOINLINE sint32 #-}
{-# NOINLINE uint64 #-}
{-# NOINLINE sint64 #-}
{-# NOINLINE pointer #-}

signed :: Storable a => a -> Type
signed proxy = case sizeOf proxy of
  1 -> sint8
  2 -> sint16
  4 -> sint32
  8 -> sint64
  _ -> unsupportedSize "signed"
{-# INLINE CONLIKE signed #-}

unsigned :: Storable a => a -> Type
unsigned proxy = case sizeOf proxy of
  1 -> uint8
  2 -> uint16
  4 -> uint32
  8 -> uint64
  _ -> unsupportedSize "unsigned"
{-# INLINE CONLIKE unsigned #-}

floating :: Storable a => a -> Type
floating proxy = case sizeOf proxy of
  4 -> float
  8 -> double
  _ -> unsupportedSize "floating"
{-# INLINE CONLIKE floating #-}

unsupportedSize :: String -> a
unsupportedSize fn = error $ "Foreign.Function.Interface.Type." ++ fn ++ ": unsupported size"
