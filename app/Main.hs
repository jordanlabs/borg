module Main where

import Lib (runRobot)

import System.IO (readFile)
import Data.Char (toUpper)
import Data.List (intercalate)

main :: IO ()
main = do
  fileContents <- readFile "data/ex1.txt"
  let result = intercalate "\n" (runRobot $ lines fileContents)
   in putStrLn result
