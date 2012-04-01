{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Foreign.Function.Signature
  ( Signature(..)
  , ret, (->:)
  ) where

import Data.Functor.Contravariant
import Foreign.Function.Interface.Type
import Foreign.Function.Interface.Util
import qualified System.IO.Unsafe as Unsafe
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
import Foreign.Ptr
import Data.Default
import Data.Hashable
import Data.Interned

data Signature = Signature
  { signatureId        :: {-# UNPACK #-} !Id
  , signatureInterface :: {-# UNPACK #-} !(Ptr CInterface)
  , signatureArity     :: {-# UNPACK #-} !Int
  , signatureABI       :: {-# UNPACK #-} !ABI
  , signatureArguments :: [Type]
  , signatureResult    :: {-# UNPACK #-} !Type
  }

instance Eq Signature where
  a == b = signatureId a == signatureId b

instance Hashable Signature where
  hash a = hash (signatureId a)
  hashWithSalt n a = hashWithSalt n (signatureId a)

instance Interned Signature where
  newtype Description Signature = DSignature (Sig Id) deriving (Eq,Hashable)
  type Uninterned Signature = Sig Type
  describe = DSignature . fmap typeId
  identity (Signature n _ _ _ _) = n
  identify slot (Sig abi args result) = unsafePerformIO $ do
    let n = length args
    cif <- mallocBytes (#sizeof ffi_cif)
    argTypes <- mallocBytes $ sizeOf (Ptr ()) * n
    let go [] _ = return ()
        go (t:ts) i = do
          pokeElemOff argTypes i (typePtr t)
          go rest $! i + 1
    go args 0
    ffi_prep_cif cif abi n (Unsafe.unsafeForeignPtrToPtr fpresult) argTypes
    return $! Signature slot cif n abi args result

-- an uninterned signature
data Sig a
  = Ret ABI a
  | a :-> Signature
  deriving (Eq,Data,Typeable)

instance Functor Sig where
  fmap f (Ret abi a) = Ret abi (f a)
  fmap f (a :-> as)  = f a :-> as

instance Hashable a => Hashable (Sig a) where
  hashWithSalt n (Ret abi result) = n `hashWithSalt` 1 `hashWithSalt` abi `hashWithSalt` result
  hashWithSalt n (arg :-> sig)    = n `hashWithSalt` 2 `hashWithSalt` arg `hashWithSalt` sig

ret :: ABI -> Type -> Signature
ret abi result = intern (Ret abi [] result)

infixr 0 ->:
(->:) :: Type -> Signature -> Signature
a ->: s = intern (a :-> s)

-- double ->: ret double
