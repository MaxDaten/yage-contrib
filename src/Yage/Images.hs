module Yage.Images
  ( module Yage.Images
  , module Codec.Picture
  ) where


import Yage.Prelude
import Yage.Math

import Control.Lens
import Codec.Picture


subImage :: (Pixel a) => Image a -> Rectangle -> Image a -> Image a
subImage sub region target 
    | check = generateImage (includeRegionImg sub region target) (imageWidth target) (imageHeight target)
    where
        includeRegionImg sub region target x y 
            | inRectangle x y region = pixelAt sub (x - region^.x0) (y - region^.y0) 
            | otherwise              = pixelAt target x y
        
        check
            | imageWidth sub  /= region^.to width ||
              imageHeight sub /= region^.to height 
             = error $ "sub image to region size mismatch"

            | imageWidth sub  > imageWidth target ||
              imageHeight sub > imageHeight target
             = error $ "sub image doesn't fit into target image"

            | not $ region `inBound` (target^.to imgRectangle)
             = error $ "region not in target bounds"

            | otherwise = True


imgRectangle :: Image a -> Rectangle
imgRectangle img = Rectangle 0 0 (imageWidth img - 1) (imageHeight img - 1)


imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imgRectangle a
        rB = imgRectangle b
    in compare rA rB