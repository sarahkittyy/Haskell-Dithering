module Error
( errorOnto
) where
    
import Prelude hiding (Left, Right)
import Graphics.Image

-- | Calculates the RGB error to add to a given pixel position.
errorOnto :: Image VS RGB Double -> (Int, Int) -> (Double, Double, Double)
errorOnto img (0, 0) = (0, 0, 0)
errorOnto img (x, y)
    | x < 0 = (0, 0, 0)
    | y < 0 = (0, 0, 0)