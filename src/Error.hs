module Error
( errorOnto
) where
    
import Prelude hiding (Left, Right)
import Graphics.Image hiding (Direction)
import RoundPixel

-- | Calculates the RGB error to add to a given pixel position.
errorOnto :: Image VS RGB Double -> (Int, Int) -> (Double, Double, Double)
errorOnto img (0, 0) = (0, 0, 0)
errorOnto img (x, y)
    | x < 0 = (0, 0, 0)
    | y < 0 = (0, 0, 0)
    | maybeIndex img (x, y) == Nothing = (0, 0, 0)
errorOnto img (x, y) =

-- | All possible directions where error can come from.
data Direction = Left | TopLeft | Above | TopRight
-- | The fraction of error of the pixel in the given direction to apply to the current pixel
errorFrac :: Direction -> Double
errorFrac dir =
    case dir of
        Left -> 7/16
        TopLeft -> 1/16
        Above -> 5/16
        TopRight -> 3/16
