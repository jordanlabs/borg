module Main where

import           Data.List          (intercalate)
import           System.Environment (getArgs)
import           System.IO          (readFile)

import           Lib                (runRobot)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (inputFilePath:_) -> do
      fileContents <- readFile inputFilePath
      let result = intercalate "\n" $ runInput fileContents
      putStrLn result
    _ -> interact (unlines . runInput)

runInput :: String -> [String]
runInput = runRobot . lines
