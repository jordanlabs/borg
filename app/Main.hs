module Main where

import           Lib       (runRobot)

import           Data.List (intercalate)
import           System.IO (readFile)

main :: IO ()
main = do
  fileContents <- readFile "data/ex1.txt"
  let result = intercalate "\n" (runRobot $ lines fileContents)
   in putStrLn result
