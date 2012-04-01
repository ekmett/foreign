{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}
module Foreign.Function.Interface.Argument
  ( Argument(..)
  ) where

import Foreign.Ptr
import Foreign.Function.Interface.Type
import Data.Functor.Contravariant

data Argument a = forall x. Argument
  { argumentType :: !Type
  , withArgument :: forall r. a -> (Ptr x -> IO r) -> IO r
  }

instance Contravariant Argument where
  contramap f (Argument t k) = Argument t (k . f)

