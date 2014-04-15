{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
-- | orphan instances for vinyl
module Data.Vinyl.Instances () where

import Yage.Prelude (Eq(..))

import Control.Monad

import Data.Binary
import Data.Vinyl
import Data.Vinyl.Idiom.Identity


instance Binary (PlainRec '[]) where
  put RNil = return ()
  get = return RNil


instance (Binary t, Binary (PlainRec fs)) => Binary (PlainRec ((sy ::: t) ': fs)) where
  put (Identity !x :& xs) = put x >> put xs
  get = do
    x <- get
    xs <- get
    return (Identity x :& xs)

instance Eq a => Eq (Identity a) where
    (Identity a) == (Identity b) = a == b