module Linear.DeepSeq () where

import Linear
import Control.DeepSeq.Generics

instance NFData a => NFData (Quaternion a) where rnf = genericRnf

instance NFData a => NFData (V4 a) where rnf = genericRnf
instance NFData a => NFData (V3 a) where rnf = genericRnf
instance NFData a => NFData (V2 a) where rnf = genericRnf
instance NFData a => NFData (V1 a) where rnf = genericRnf
instance NFData a => NFData (V0 a) where rnf = genericRnf