{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Foreign.Function.Struct
  ( Struct(..)
  , head
  , tail
  , reverse, Reverse
  , append, Append
  , All
  ) where

import Prelude hiding (head,tail,reverse)
import Foreign.Storable
-- import Text.Read
import GHC.Prim (Constraint)

infixr 5 :&

-- a struct is just an HList
data Struct :: [*] -> * where
  Nil  :: Struct '[]
  (:&) :: a -> Struct as -> Struct (a ': as)

head :: Struct (a ': as) -> a
head (x :& _) = x

tail :: Struct (a ': as) -> Struct as
tail (_ :& xs) = xs

type family Reverse' (acc :: [*]) (as :: [*]) :: [*]
type instance Reverse' acc '[] = acc
type instance Reverse' acc (a ': as) = Reverse' (a ': acc) as

reverse' :: Struct acc -> Struct as -> Struct (Reverse' acc as)
reverse' acc Nil = acc
reverse' acc (a :& as) = reverse' (a :& acc) as

type family Reverse (as :: [*]) :: [*]
type instance Reverse as = Reverse' '[] as

reverse :: Struct as -> Struct (Reverse as)
reverse as = reverse' Nil as

type family All (p :: * -> Constraint) (xs :: [*]) :: Constraint
type instance All p '[]       = ()
type instance All p (a ': as) = (p a, All p as)

instance All Show as => Show (Struct as) where
  showsPrec d (a :& as) = showParen (d > 5) $
    showsPrec 6 a . showString " :& " . showsPrec 5 as
  showsPrec _ Nil = showString "Nil"

type family Append (as :: [*]) (bs :: [*]) :: [*]
type instance Append '[]       bs = bs
type instance Append (a ': as) bs = a ': Append as bs
append :: Struct as -> Struct bs -> Struct (Append as bs)
append Nil       bs = bs
append (a :& as) bs = a :& append as bs

{-
delta :: Storable a => Int -> a -> Int
delta p a = m - mod p m where m = max (alignment a) 1

class Delta as where
  delta :: Int -> Struct as -> Int

instance Delta '[] where
  delta _ _ = 0

instance Storable a => Delta (a ': as) where
  delta n as = m - mod n m where m = alignment (head as)
-}

-- TODO: use deltas to maintain alignment!
instance (Storable a, Storable (Struct as)) => Storable (Struct (a ': as)) where
  sizeOf as    = sizeOf (head as) + sizeOf (tail as)
  alignment as = alignment (head as) `max` alignment (tail as)
  peekByteOff p n = do
    a <- peekByteOff p n
    as <- peekByteOff p (n + sizeOf a)
    return $! a :& as
  pokeByteOff p n (a :& as) = do
    pokeByteOff p n a
    pokeByteOff p (n + sizeOf a) as

instance Storable (Struct '[]) where
  sizeOf _    = 0
  alignment _ = 1
  peekByteOff _ _ = return Nil
  pokeByteOff _ _ _ = return ()

