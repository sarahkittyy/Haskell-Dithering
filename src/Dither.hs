module Dither
( dither
) where

import Prelude hiding (Right, Left, Either, error)
import Graphics.Image hiding (Direction, RGB, map, transpose)
import qualified Graphics.Image as GI (RGB)
import Graphics.Image.Interface hiding (map, transpose)
import Data.List (transpose)

dither :: Image VS GI.RGB Double -> Image VS GI.RGB Double
dither = fromLists . map (map toPixel) . processPixels . map (map fromPixel) . toLists

-- | Maps the function over all pixels, left->right, top->down
--- This is also the same as 
mapPixels :: (RGB -> RGB) -> [[RGB]] -> [[RGB]]
mapPixels fn rgb = (map $ map fn) $ transpose rgb

processPixels :: [[RGB]] -> [[RGB]]
processPixels = mapPixels (\rgb -> rgb `rgbScale` 0.5)
    
closestColor :: RGB -> RGB
closestColor (r, g, b) = ((*ct) . fromIntegral . round . (/ct)) `map3` (r, g, b)
    where
        ct = 127


-- | map over a 3-tuple
map3 :: (a -> a) -> (a, a, a) -> (a, a, a) 
map3 fn (a, b, c) = (fn a, fn b, fn c)

type RGB = (Double, Double, Double)

rgbSub :: RGB -> RGB -> RGB
(r1, g1, b1) `rgbSub` (r2, b2, g2) = (r1 - r2, g1 - g2, b1 - b2)

rgbScale :: RGB -> Double -> RGB
(r1, g1, b1) `rgbScale` c  = (c * r1, c * g1, c * b1)

data Direction = Right | BottomLeft | Bottom | BottomRight

error :: Direction -> RGB -> RGB -> RGB
error dir oldPixel newPixel = qerror `rgbScale` frac
    where
        frac = (/16) $ case dir of
                        Right -> 7
                        BottomLeft -> 3
                        Bottom -> 5
                        BottomRight -> 1
                
        qerror = oldPixel `rgbSub` newPixel
        
toPixel :: RGB -> Pixel GI.RGB Double
toPixel (r, g, b) = PixelRGB (r/255) (g/255) (b/255)

fromPixel :: Pixel GI.RGB Double -> RGB
fromPixel (PixelRGB r g b) = (*255) `map3` (r, g, b)

tuplize3 :: a -> (a, a, a)
tuplize3 a = (a, a, a)