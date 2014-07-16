{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
-- | orphan instances for vinyl
module Data.Vinyl.Instances () where

import Yage.Prelude (Eq(..), NFData(..), seq, (<$>))

import Control.Monad

import Data.Binary
import Data.Vinyl
import Data.Vinyl.TyFun
import Data.Vinyl.Idiom.Identity


instance Binary (Rec el f '[]) where
  put RNil = return ()
  get = return RNil


instance (Binary (f (el $ r)), Binary (Rec el f rs)) 
    => Binary (Rec el f (r ': rs)) where
  put (!x :& xs) = put x >> put xs
  get = do
    x <- get
    xs <- get
    return (x :& xs)

instance Binary a => Binary (Identity a) where
  put (Identity x) = put x
  get = Identity <$> get

instance Eq a => Eq (Identity a) where
  (Identity a) == (Identity b) = a == b


instance NFData a => NFData (Identity a) where
  rnf (Identity x) = x `seq` ()

instance NFData (Rec el f '[]) where

instance (NFData (f (el $ r)), NFData (Rec el f rs)) => NFData (Rec el f (r ': rs)) where
    rnf (x :& xs) = x `seq` rnf xs