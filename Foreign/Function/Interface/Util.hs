{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.Function.Interface.Util (addForeignPtrHaskellFinalizer) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Data.IORef
import Control.Monad.Fix

foreign import ccall "wrapper" wrapFinalizerPtr :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

-- | Flipped arguments relative to addForeignPtrFinalizer because its a lot nicer with @$ do@ at the end
addForeignPtrHaskellFinalizer :: ForeignPtr a -> (Ptr a -> IO ()) -> IO ()
addForeignPtrHaskellFinalizer p f = do
  w <- mfix $ \w -> wrapFinalizerPtr $ \ p' -> do
    r <- f p'
    freeHaskellFunPtr w
    return r
  addForeignPtrFinalizer w p
