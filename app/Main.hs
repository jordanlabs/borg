module Main where

-- import Lib (runRobot)

import System.IO (readFile)

main :: IO ()
main = do
  fileContents <- readFile "data/ex1.txt"
  putStrLn fileContents

-- commands <- loadfile
--
