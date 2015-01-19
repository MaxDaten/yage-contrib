{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable        #-}
module Foreign.Storable.Utils where

import Yage.Prelude

import Foreign.Storable
import Foreign.Ptr
import Data.Typeable


instance Storable () where
    sizeOf _ = 0
    peek _ = return ()
    alignment _ = 0
    poke _ _ = return ()


data SomeStorable = forall s. (Storable s, Typeable s) => SomeStorable s
    deriving (Typeable)

instance Show SomeStorable where
    show (SomeStorable s) = printf "SomeStorable { alignment: %d, size: %d }" (alignment s) (sizeOf s)

instance Storable SomeStorable where
    sizeOf (SomeStorable s) = sizeOf s
    alignment (SomeStorable s) = alignment s
    peek = error "peek not implemented for SomeStorable"
    poke ptr (SomeStorable s) = poke (castPtr ptr) s


toStorable :: (Typeable a, Storable a) => a -> SomeStorable
toStorable = SomeStorable

fromStorable :: (Typeable s, Storable s) => SomeStorable -> Maybe s
fromStorable (SomeStorable s) = cast s
