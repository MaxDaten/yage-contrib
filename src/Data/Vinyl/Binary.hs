{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
module Data.Vinyl.Binary () where

import Data.Binary
import Data.Vinyl
import Control.Monad
import Data.Functor.Identity


instance Binary (PlainRec '[]) where
  put RNil = return ()
  get = return RNil

instance (Binary t, Binary (PlainRec fs)) => Binary (PlainRec ((sy ::: t) ': fs)) where
  put (Identity !x :& xs) = put x >> put xs
  get = do
    x <- get
    xs <- get
    return (Identity x :& xs)