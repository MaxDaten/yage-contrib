{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.C.Types.Binary where

import Yage.Prelude
import Foreign.C.Types
import Data.Binary

instance Binary CFloat where
    get = CFloat <$> (get :: Get Float)
    put (CFloat f) = put f