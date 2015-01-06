{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleInstances      #-}
-- |A bridge between the JuicyPixels and colour packages
-- will result in a own module later on
module JuicySRGB where

import Prelude

import Control.Monad (liftM, ap)
import Codec.Picture
import Codec.Picture.Types
import Data.Word (Word8)
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SRGB
import qualified Data.Colour.SRGB.Linear as Linear
import qualified Data.Colour.RGBSpace as C

import Foreign.Storable ( Storable )
import Control.Monad.Primitive ( PrimMonad, PrimState )

import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


-- | lifts the PixelRGB8 to an PixelSRGB8
newtype PixelSRGB8 = PixelSRGB8 PixelRGB8
    deriving ( Eq, Ord, Show )

instance Pixel PixelSRGB8 where
    type PixelBaseComponent PixelSRGB8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelSRGB8 a) (PixelSRGB8 b) = PixelSRGB8 $ mixWith f a b

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE colorMap #-}
    colorMap f (PixelSRGB8 a) = PixelSRGB8 $ colorMap f a

    pixelAt image@(Image { imageData = arr }) x y = PixelSRGB8 $ PixelRGB8  (arr ! (baseIdx + 0))
                                                                            (arr ! (baseIdx + 1))
                                                                            (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr .!!!. baseIdx
        gv <- arr .!!!. (baseIdx + 1)
        bv <- arr .!!!. (baseIdx + 2)
        return $ PixelSRGB8 $ PixelRGB8 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelSRGB8 (PixelRGB8 rv gv bv)) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr .<-. (baseIdx + 0)) rv
        (arr .<-. (baseIdx + 1)) gv
        (arr .<-. (baseIdx + 2)) bv

    unsafePixelAt v idx =
        PixelSRGB8 $ PixelRGB8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)

    unsafeReadPixel vec idx =
        mkPixelSRGB8 `liftM` M.unsafeRead vec idx
                        `ap` M.unsafeRead vec (idx + 1)
                        `ap` M.unsafeRead vec (idx + 2)

    unsafeWritePixel v idx (PixelSRGB8 (PixelRGB8 r g b)) = do
        M.unsafeWrite v idx r
        M.unsafeWrite v (idx + 1) g
        M.unsafeWrite v (idx + 2) b

-- |converts from the linear RGB space to the non-linear Standard-RGB (sRGB) color space
-- approxumates a gamma of about 1/2.2
-- see: http://hackage.haskell.org/package/colour-2.3.3/docs/src/Data-Colour-SRGB.html#sRGB24
instance ColorSpaceConvertible PixelRGB8 PixelSRGB8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB8 r g b) = PixelSRGB8 . C.uncurryRGB PixelRGB8 . SRGB.toSRGB24 . asDouble $ linRGB24 r g b
        where
        asDouble :: C.Colour Double -> C.Colour Double
        asDouble = id


instance ColorSpaceConvertible PixelSRGB8 PixelRGB8 where
    {-# INLINE convertPixel #-}
    -- sRGB24 transfers the sRGB color coordinates to the linear Colour type
    -- toRGB simply exposes the true values of the color-channels
    convertPixel (PixelSRGB8 (PixelRGB8 r g b)) = C.uncurryRGB PixelRGB8 . toLinearBounded . asDouble $ SRGB.sRGB24 r g b
        where
        asDouble :: C.Colour Double -> C.Colour Double
        asDouble = id


-- |a description how to convert a `Colour` type to the concrete pixel
class (Pixel p, Floating a) => ColourPixel a p where
    colourToPixel :: C.Colour a -> p


instance (Floating a, RealFrac a) => ColourPixel a PixelSRGB8 where
    colourToPixel = PixelSRGB8 . C.uncurryRGB PixelRGB8 . SRGB.toSRGB24

instance (Floating a, RealFrac a) => ColourPixel a PixelRGB8 where
    colourToPixel = C.uncurryRGB PixelRGB8 . toLinearBounded

instance (Floating a, RealFrac a) => ColourPixel a PixelRGBA8 where
    colourToPixel = C.uncurryRGB (\r g b -> PixelRGBA8 r g b 1) . toLinearBounded

instance (Floating a, RealFrac a) => ColourPixel a Pixel8 where
    colourToPixel c = computeLuma (colourToPixel c :: PixelRGB8)

-- just helpers

castToSRGB8 :: Image PixelRGB8 -> Image PixelSRGB8
castToSRGB8 = pixelMap PixelSRGB8

castToRGB8 :: Image PixelSRGB8 -> Image PixelRGB8
castToRGB8 = pixelMap (\(PixelSRGB8 pixelRGB) -> pixelRGB)

{-# INLINE (!!!) #-}
(!!!) :: (Storable e) => V.Vector e -> Int -> e
(!!!) = V.unsafeIndex


{-# INLINE (.!!!.) #-}
(.!!!.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> m a
(.!!!.) = M.read -- unsafeRead


{-# INLINE (.<-.) #-}
(.<-.) :: (PrimMonad m, Storable a) => M.STVector (PrimState m) a -> Int -> a -> m ()
(.<-.)  = M.write -- unsafeWrite



{-# INLINE mkPixelSRGB8 #-}
mkPixelSRGB8 :: Pixel8 -> Pixel8 -> Pixel8 -> PixelSRGB8
mkPixelSRGB8 r g b = PixelSRGB8 $ PixelRGB8 r g b


-- |lifts linear color coordiantes `r` `g` `b` into the Colour type (which is linear by definition)
-- the values are normalized with their maxBound
-- no transfer function is applied
{-# INLINE linRGB24 #-}
linRGB24 :: (Ord b, Floating b, Integral a, Bounded a) =>
            a -> a -> a -> C.Colour b
linRGB24 r g b = C.uncurryRGB Linear.rgb $ fmap normalize $ Linear.RGB r g b
    where
    normalize x = fromIntegral x / m
    m = fromIntegral $ maxBound `asTypeOf` r


{- Results are clamped and quantized, without transfer -}
-- |Return the approximate sRGB colour components in the range
-- [0..'maxBound'].
-- Out of range values are clamped.
toLinearBounded :: (RealFrac b, Floating b, Integral a, Bounded a) =>
                   C.Colour b -> Linear.RGB a
toLinearBounded c = fmap quant (Linear.toRGB c)
    where
    quant x' = quantize (m*x')
    m = fromIntegral $ maxBound `asTypeOf` (quant undefined)


-- |'round's and then clamps @x@ between 0 and 'maxBound'.
-- from: http://hackage.haskell.org/package/colour-2.3.3/src/Data/Colour/Internal.hs
quantize :: (RealFrac a1, Integral a, Bounded a) => a1 -> a
quantize x | x <= fromIntegral l = l
           | fromIntegral h <= x = h
           | otherwise           = round x
    where
    l = minBound
    h = maxBound
