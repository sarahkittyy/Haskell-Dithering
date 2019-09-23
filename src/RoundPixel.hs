module RoundPixel
( roundPixel
) where
    
import Prelude (floor, Double, (/), (*), fromIntegral)
import Graphics.Image

-- | Rounds a pixel to the nearest "pallate color" -- for floyd-steinberg dithering.    
roundPixel :: Pixel RGB Double -> Pixel RGB Double
roundPixel (PixelRGB r g b) = (PixelRGB (rf / 255) (gf / 255) (bf / 255))
    where
        r255 = r * 255
        g255 = g * 255
        b255 = b * 255
        rf = roundComponent r255
        gf = roundComponent g255
        bf = roundComponent b255
        
roundComponent :: Double -> Double
roundComponent x = fromIntegral (floor (x / cf)) * cf
            where
                cf = 126