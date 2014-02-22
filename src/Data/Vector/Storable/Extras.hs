module Data.Vector.Storable.Extras where

import Prelude (otherwise, ($))
import Data.List as L (map, unzip, all)
import Data.Vector.Storable

-- NOT very efficent
interleave :: (Storable a) => [Vector a] -> Vector a
interleave vs
    | L.all null vs = empty
    | otherwise     = let (as, vss)  = L.unzip $ L.map (splitAt 1) vs
                      in concat as  ++ interleave vss


