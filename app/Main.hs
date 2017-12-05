module Main where

import           Data.List          (intercalate)
import           Data.Maybe         (listToMaybe)
import           System.Environment (getArgs)
import           System.IO          (readFile)

import           Lib                (runRobot)

main :: IO ()
main = do
  maybeFilePath <- listToMaybe <$> getArgs
  case maybeFilePath of
    Just filePath -> do
      fileContents <- readFile filePath
      let result = intercalate "\n" $ runInput fileContents
      putStrLn result
    _ -> interact (unlines . runInput)

runInput :: String -> [String]
runInput = runRobot . lines
