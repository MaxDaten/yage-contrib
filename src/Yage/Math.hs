{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Yage.Math
    ( module Yage.Math
    , module Linear
    ) where

import Yage.Prelude hiding ((++))
import Yage.Lens
import Yage.Data.List hiding (any, map, sum)
import Data.Binary
import Linear
import Linear.Binary

xAxis, yAxis, zAxis :: (Fractional a) => V3 a
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- | a plain in 3d space in plain normal form 
type Plain3DNF a = (V3 a, V3 a, V3 a)

infixl 7 ><

(><):: Num a => V3 a -> V3 a -> V3 a
(><) = cross

-- | Convert degrees to radians.
deg2rad :: RealFloat a => a -> a
deg2rad x = x * pi / 180


rad2deg :: RealFloat a => a -> a
rad2deg x = x * 180 / pi

clamp :: Ord a => a -> a -> a -> a
clamp upper lower = max lower . min upper

fromTransformation :: M44 a -> M33 a
fromTransformation
    (V4 (V4 a b c _)
        (V4 d e f _)
        (V4 g h i _)
        (V4 _ _ _ _)) = V3 (V3 a b c)
                           (V3 d e f)
                           (V3 g h i)

-- | calculates a normal from two vectors in local space (no position vectors)
normal :: (Num a, Floating a, Epsilon a) => V3 a -> V3 a -> V3 a
normal v1 v2 = normalize $ cross v1 v2


-- | calculates a plain in normal form from three position vectors
plainNormalForm :: (Num a, Floating a, Epsilon a) => V3 a -> V3 a -> V3 a -> Plain3DNF a
plainNormalForm v1 v2 v3 = 
    let n = normal (v2 - v1) (v2 - v3)
        p = v1
        q = v2
    in (n, p, q)

-- possible not the fastest implementation
normals :: (Num a, Floating a, Epsilon a) => [V3 a] -> [V3 a]
normals vs = map norms $ chunksOf 3 vs
    where
    norms (a:b:c:[]) = (plainNormalForm a b c)^._1


-- | normalized sum vector
averageNorm :: (Num (f a), Epsilon a, Metric f, Floating a) => [f a] -> f a
averageNorm = normalize . sum

-- | uses the Gram-Schmidt to create a orthogonal basis 
orthogonalize :: (Num a) => V3 (V3 a) -> V3 (V3 a)
orthogonalize (V3 n t b) =
    let t' = t - (n `dot` t)*^n
        b' = b - (n `dot` b)*^n - (t' `dot` b)*^t'
    in V3 n t' b'


orthonormalize :: (Num a, Epsilon a, Floating a) => V3 (V3 a) -> V3 (V3 a)
orthonormalize = fmap normalize . orthogonalize

{--
genNormals :: (Num a, Show a, Floating a) => [V3 a] -> [V3 a]
genNormals vs =
    let ns = concat $ map (\(a:b:_) -> replicate 3 $ a `normal` b) $ splitEvery 3 vs
        cs = repeat color
    in zipWith3 Vertex vs ns cs
--}

-- isValid :: (RealFloat c, MonoFoldable c) => c -> Bool
isValid :: (MonoFoldable c, RealFloat t, Element c ~ t) => c -> Bool
isValid a = any (\b -> isNaN b || isInfinite b) a

---------------------------------------------------------------------------------------------------
instance Binary a => Binary (V1 a) where
    put = putLinear
    get = getLinear

instance Binary a => Binary (V2 a) where
    put = putLinear
    get = getLinear

instance Binary a => Binary (V3 a) where
    put = putLinear
    get = getLinear

instance Binary a => Binary (V4 a) where
    put = putLinear
    get = getLinear
