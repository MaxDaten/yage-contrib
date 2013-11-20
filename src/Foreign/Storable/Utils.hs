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
    show (SomeStorable s) = format "SomeStorable { alignment: {0}, size: {1} }" [show . alignment $ s, show . sizeOf $ s] 

instance Storable SomeStorable where
    sizeOf (SomeStorable s) = sizeOf s
    alignment (SomeStorable s) = alignment s
    --peek = peek
    poke ptr (SomeStorable s) = poke (castPtr ptr) s


toStorable :: (Typeable a, Storable a) => a -> SomeStorable
toStorable = SomeStorable

fromStorable :: (Typeable s, Storable s) => SomeStorable -> Maybe s
fromStorable (SomeStorable s) = cast s
