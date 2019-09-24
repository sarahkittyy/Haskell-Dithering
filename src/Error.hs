module Error
( errorOnto
) where
    
import Prelude hiding (Left, Right)
import Graphics.Image hiding (Direction, sum, map)
import RoundPixel

-- | Scale a 3-tuple
(***) :: (Num x) => (x, x, x) -> x -> (x, x, x)
(***) (x1, y1, z1) c = (x1 * c, y1 * c, z1 * c)

-- | Sum two tuples
addTuples :: (Num x) => (x, x, x) -> (x, x, x) -> (x, x, x)
addTuples (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- | Subtract two tuples
subTuples :: (Num x) => (x, x, x) -> (x, x, x) -> (x, x, x)
subTuples (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- | Sum a list of tuples
sumTuples :: (Num x) => [(x, x, x)] -> (x, x, x)
sumTuples = foldl addTuples (0, 0, 0)

-- | Calculates the RGB error to add to a given pixel position.
errorOnto :: Image VS RGB Double -> (Int, Int) -> (Double, Double, Double)
errorOnto img (0, 0) = (0, 0, 0)
errorOnto img (x, y)
    | x < 0 = (0, 0, 0)
    | y < 0 = (0, 0, 0)
    | maybeIndex img (x, y) == Nothing = (0, 0, 0)
errorOnto img (x, y) =
    let
        errormap = map (toTuple . roundPixel . fromTuple) $ map (errorOnto img) $ map (flip newCoords (x, y)) [Left, TopLeft, Above, TopRight]
        oldpx = toTuple $ index img (x, y)
        newpx = toTuple $ roundPixel (index img (x, y))
        err = oldpx `subTuples` newpx
    in
        err `addTuples` (sumTuples $ zipMap ((flip (***)) . errorFrac) [Left, TopLeft, Above, TopRight] errormap)
        
zipMap :: (a -> b -> c) -> [a] -> [b] -> [c]
zipMap fn [] l2 = []
zipMap fn l1 [] = []
zipMap fn (x:xs) (y:ys) = (fn x y):zipMap fn xs ys
    
-- | Takes a direction and a pair of integers and translates the coordinates in the direction
newCoords :: Direction -> (Int, Int) -> (Int, Int)
newCoords dir (x, y) =
    case dir of
        Left -> (x - 1, y)
        TopLeft -> (x - 1, y - 1)
        Above -> (x, y - 1)
        TopRight -> (x + 1, y - 1)
        
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
