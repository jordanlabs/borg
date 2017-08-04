module Main where

-- import Lib (runRobot)

import System.IO (readFile)
import Data.Char (toUpper)

main :: IO ()
main = do
  fileContents <- readFile "data/ex1.txt"
  putStrLn fileContents
