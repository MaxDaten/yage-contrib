{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Yage.Math
    ( module Yage.Math
    , module Linear
    ) where

import Yage.Prelude hiding ((++))
import Yage.Lens
import Yage.Data.List hiding (any, map, sum)
import Yage.Math.Instances ()
import Data.Binary
import Linear

xAxis, yAxis, zAxis :: (Fractional a) => V3 a
xAxis = V3 1 0 0
yAxis = V3 0 1 0
zAxis = V3 0 0 1

-- | a plain in 3d space in plain normal form
type Plain3DNF a = (V3 a, V3 a, V3 a)

infixl 7 ><

(><) :: Num a => V3 a -> V3 a -> V3 a
(><) = cross
{-# INLINE (><) #-}


-- | Convert degrees to radians.
deg2rad :: RealFloat a => a -> a
deg2rad x = x * pi / 180
{-# SPECIALIZE INLINE deg2rad :: Float -> Float #-}
{-# SPECIALIZE INLINE deg2rad :: Double -> Double #-}


rad2deg :: RealFloat a => a -> a
rad2deg x = x * 180 / pi
{-# SPECIALIZE INLINE rad2deg :: Float -> Float #-}
{-# SPECIALIZE INLINE rad2deg :: Double -> Double #-}


clamp :: Ord a => a -> a -> a -> a
clamp val lower upper = max lower . min upper $ val
{-# SPECIALIZE INLINE clamp :: Int -> Int -> Int -> Int #-}
{-# SPECIALIZE INLINE clamp :: Float -> Float -> Float -> Float #-}
{-# SPECIALIZE INLINE clamp :: Double -> Double -> Double -> Double #-}

fmod :: forall a. RealFrac a => a -> a -> a
fmod a b = b * snd (properFraction $ a / b :: (Int, a))
{-# SPECIALIZE INLINE fmod :: Float -> Float -> Float #-}
{-# SPECIALIZE INLINE fmod :: Double -> Double -> Double #-}


-- | convert a 4x4 matrix to a 3x3 matrix by dropping
-- last column and row
--
-- >>> m44_to_m33 (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p))
-- V3 (V3 a b c) (V3 e f g) (V3 i j k)
m44_to_m33 :: M44 a -> M33 a
m44_to_m33
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
    norms _ = error "Yage.Math.normals"


-- | normalized sum vector
averageNorm :: (Num (v a), Epsilon a, Metric v, Floating a, MonoFoldable f, Element f ~ (v a)) => f -> v a
averageNorm = normalize . sum

-- | uses the Gram-Schmidt to create a orthogonal basis
orthogonalize :: (Num a) =>
    (M33 a) ->
    -- ^ Basis (Tangent (x), Bitangent (y), Normal (z))
    (M33 a)
    -- ^ resulting orthogonal basis (without normalization)
orthogonalize (V3 t b n) =
    let t' = t - (n `dot` t)*^n
        b' = b - (n `dot` b)*^n - (t' `dot` b)*^t'
    in V3 t' b' n


orthonormalize :: (Num a, Epsilon a, Floating a) => M33 a -> M33 a
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

{--
OpenGL Aliases
--}

-- Textures Coords

_s :: R1 t => Lens' (t a) a
_s = _x

_t :: R2 t => Lens' (t a) a
_t = _y

_p :: R3 t => Lens' (t a) a
_p = _z

_q :: R4 t => Lens' (t a) a
_q = _w


_st :: R2 t => Lens' (t a) (V2 a)
_st = _xy

_stp :: R3 t => Lens' (t a) (V3 a)
_stp = _xyz
