{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
module Foreign.Function.Interface.Result
  ( Result(..)
  ) where

import Foreign.Ptr
import Foreign.Function.Interface.Type

data Result a = forall x. Result
  { resultType   :: !Type
  , resultNew    :: IO (Ptr x)
  , resultFinish :: forall r. (a -> Ptr x -> IO r) -> IO r
  }

instance Functor Result where
  fmap f (Result t new kf) = Result t new (\k -> kf (k . f))

