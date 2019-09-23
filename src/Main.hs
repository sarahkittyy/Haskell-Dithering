module Main where
    
import Dither
import System.Environment
import Graphics.Image hiding (map)
import Data.Char
import Graphics.Image.IO

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    if (length args /= 1)
        then putStrLn $ "Usage: " ++ prog ++ " <file>"
        else do
            img <- readImage (head args) :: IO (Either String (Image VS RGB Double))
            case img of
                (Left a) -> putStrLn $ "Error in opening image: " ++ a ++ "\n"
                (Right image) -> do
                    putStrLn "Processing..."
                    writeImage (toOutput $ head args) . dither $ image
                    putStrLn "Done!"
                
toOutput :: String -> String
toOutput str =
    let split = map ($ str) $ ($ (/='.')) `map` [takeWhile, dropWhile]
    in head split ++ ".out" ++ last split