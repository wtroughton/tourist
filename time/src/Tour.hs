module Tour (someFunc) where

import Data.Time

someFunc :: IO ()
someFunc = do
    currentTime <- getCurrentTime
    putStrLn $ show currentTime
