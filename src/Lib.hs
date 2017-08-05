module Lib
    ( runRobot
    ) where

import Parser

import Data.Maybe (catMaybes)

runRobot :: [String] -> [String]
runRobot input = do
  command <- catMaybes $ fmap parseCommand input
  ["Woah"]
