{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Yage.Math where

import Yage.Prelude
import Control.Lens
import Data.List (map)
import Linear (V2(..), V3(..), V4(..), M33, M44, cross, normalize, Epsilon)

zero, one :: (Floating a) => a
zero = 0.0
one  = 1.0

uv00, uv01, uv10, uv11 :: (Floating a) => V2 a
uv00  = V2 zero zero 
uv01  = V2 zero one
uv10  = V2 one zero
uv11  = V2 one one

xAxis, yAxis, zAxis :: (Floating a) => V3 a
xAxis = V3 one zero zero
yAxis = V3 zero one zero
zAxis = V3 zero zero one

type Normal a = V3 a
-- | a plain in 3d space in plain normal form 
type Plain3DNF a = (Normal a, V3 a, V3 a)

infixl 7 ><

(><):: Num a => V3 a -> V3 a -> V3 a
(><) = cross

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
normals vs = map norms $ splitEvery 3 vs
    where
        norms (a:b:c:[]) = (plainNormalForm a b c)^._1

{--
genNormals :: (Num a, Show a, Floating a) => [V3 a] -> [V3 a]
genNormals vs =
    let ns = concat $ map (\(a:b:_) -> replicate 3 $ a `normal` b) $ splitEvery 3 vs
        cs = repeat color
    in zipWith3 Vertex vs ns cs
--}

---------------------------------------------------------------------------------------------------

data Rectangle = Rectangle
    { _x0, _y0, _x1, _y1 :: !Int }
    deriving (Show)
makeLenses ''Rectangle

width rect  = rect^.x1 - rect^.x0 + 1
height rect = rect^.y1 - rect^.y0 + 1

instance Eq Rectangle where
    r1 == r2 = 
       r1^.x0 == r2^.x0 && 
       r1^.y0 == r2^.y0 &&
       r1^.x1 == r2^.x1 &&
       r1^.y1 == r2^.y1


instance Ord Rectangle where
    compare r1 r2 = 
        let w1 = r1^.to width
            h1 = r1^.to height
            w2 = r2^.to width
            h2 = r2^.to height
        in compare (w1*h1) (w2*h2)



fits :: Rectangle -> Rectangle -> Bool
fits rect1 rect2 = rect1^.to width <= rect2^.to width && 
                   rect1^.to height <= rect2^.to height

dimMatches :: Rectangle -> Rectangle -> Bool
dimMatches a b =
    a^.to width  == b^.to width &&
    a^.to height == b^.to height

inBound :: Rectangle -> Rectangle -> Bool
inBound inner outer = 
    inner^.x1 <= outer^.x1 && inner^.x0 >= outer^.x0 &&
    inner^.y1 <= outer^.y1 && inner^.y0 >= outer^.y0


inRectangle :: Int -> Int -> Rectangle -> Bool
inRectangle x y rect =
    x >= rect^.x0 && x <= rect^.x1 &&
    y >= rect^.y0 && y <= rect^.y1