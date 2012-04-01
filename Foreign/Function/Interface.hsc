{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Foreign.Function.Interface
  ( Argument(..)
  , struct
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
  data Description Signature = DSignature ABI [Id] Id deriving (Eq)
  type Uninterned Signature = Sig
  describe (Sig abi args result) = DSignature abi (map typeId args) (typeId result)
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
    return $! Signature slot cif abi args result

instance Hashable (Description Signature) where
  hashWithSalt n (DSignature abi xs  = n `hashWithSalt` 

-- a representation of a list of arguments and a result, sans abi
data Sig = Sig ABI [Type] Type deriving (Eq,Data,Typeable)

instance Hashable Sig where
  hashWithSalt n (Sg as r) = n `hashWithSalt` as `hashWithSalt` r

sig :: ABI -> Type -> Sig
sig = Sig abi []

infixr 0 ->
(~>) :: Type -> Sig -> Sig
a ~> Fun as r = Fun (a:as) r

data CInterface deriving Typeable

instance Foreign

data Proxy a = Proxy

resultProxy :: p (a -> b) -> Proxy b
resultProxy _ = Proxy

resultSig :: Foreign b => p (a -> b) -> Sig
resultSig p = proxySig (resultProxy p)

signature :: Foreign a => ABI -> p a -> Signature
signature abi p = case proxySig p of
  Sig args result -> intern (abi,args,result)

class Foreign a where
  proxySig :: p a -> Sig

instance Foreign Double where
  proxySig _ = sig double

instance Foreign Int8 where
  proxySig _ = sig sint8

instance Foreign Word8 where
  proxySig _ = sig uint8

instance Foreign a => Foreign (Double -> a) where
  proxySig p = double ~> resultSig p

call :: Foreign a => ABI -> FunPtr a -> a
call abi fp = signature abi fp (unsafeCall (pack i)

class Foreign a where
  pack   :: [Ptr CValue] -> proxy a -> IO r
  unpack :: ([Ptr CValue] -> [Ptr CValue]) -> IO a

instance Foreign Int where
  unfurl _ = ([], signed (undefined :: Int))

instance Foreign

sig :: ABI -> [Type] -> Type -> Sig

data Signature = Signature [
  = Arg 
    { signatureId      :: {-# UNPACK #-} !Id 
    , _signatureArgs   :: {-# UNPACK #-} !Int
    , signatureABI     :: {-# UNPACK #-} !Int
    , signatureCif     :: {-# UNPACK #-} !(Ptr FFI_CIF)
    , signatureArgType :: !Type
    , signatureRest    :: !Signature
  | Result {-# UNPACK #-} !Id {-# UNPACK #-} !Type 
  = Signature Id (Ptr FFI_CIF) [Type] ABI Type

signature :: 

(->:) Type -> Signature -> Signature

instance

data Interface a where
  Interface Signature Manipulation

data Argument a = forall x. Argument
  { argumentType :: !Type
  , withArgument :: forall r. a -> (Ptr x -> IO r) -> IO r
  }

instance Contravariant Argument where
  contramap f (Argument t k) = Argument t (k . f)

data Result a = forall x. Result
  { resultType   :: !Type
  , resultNew    :: IO (Ptr x)
  , resultFinish :: forall r. (a -> Ptr x -> IO r) -> IO r
  }

instance Functor Result where
  fmap f (Result t new kf) = Result t new (\k -> kf (k . f))


data CIF deriving Typeable

data Signature a where
  Result :: Type -> (forall r. (Ptr a -> IO r) -> IO r) -> Signature a
  

data Arg a = Arg
  { argType :: Type
  , forall r. (Ptr CValue -> IO r) -> IO r
  }

data Result a = Result
  { resultType :: Type
  , forall r. (Ptr CValue -> IO a)
  }

newtype Interface a = ForeignFunction (ForeignPtr CIF)

newtype ABI = ABI Int

-- TODO: build a global signature hashtable

call :: Foreign a => FunPtr a -> a

prep :: ABI -> [Type] -> Type -> Interface a
prep (ABI abi) args result@(Type fpresult) = Unsafe.unsafeDupablePerformIO $ do
  let n = length args
  types <- newStablePtr (args,result)
  fcif <- mallocForeignPtrBytes (#sizeof ffi_cif)
  argTypes <- mallocBytes $ sizeOf (Ptr Type) * n
  let go [] _ = return ()
      go (Type fp:rest) i = do
        pokeElemOff argTypes i (Unsafe.unsafeForeignPtrToPtr fp)
        go rest $! i + 1
  go args 0
  withForeignPtr fcif $ \pcif ->
    ffi_prep_cif pcif abi n (Unsafe.unsafeForeignPtrToPtr fpresult) argTypes
  addForeignPtrHaskellFinalizer fcif $ do
    free argTypes
    freeStablePtr types
  return fcif


-- class Arg a where
--  argType :: Type
--  withArg :: a -> (Ptr () -> IO b) -> IO b
