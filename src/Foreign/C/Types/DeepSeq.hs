{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foreign.C.Types.DeepSeq where

import Foreign.C.Types
import Control.DeepSeq

instance NFData CFloat where 