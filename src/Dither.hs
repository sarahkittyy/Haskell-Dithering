module Dither
( dither
) where

import Prelude (Double, Int, (+))
import Graphics.Image
import Graphics.Image.Interface
import Error

-- | Applies the floyd-steinberg dithering algorithm to the image
dither :: Image VS RGB Double -> Image VS RGB Double
dither img = imap (transformPixel img) img

-- | Applies the error recursively generated from all previously neighboring pixels onto the given pixel.
transformPixel :: Image VS RGB Double -> (Int, Int) -> Pixel RGB Double -> Pixel RGB Double
transformPixel img (x, y) (PixelRGB r g b) =
    let
        (errorR, errorG, errorB) = errorOnto img (x, y)
    in
        (PixelRGB (r + errorR) (g + errorG) (b + errorB))