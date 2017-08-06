module Lib
    ( runRobot
    ) where

import Parser
import Types
import BoardProcessor

import Data.Maybe (catMaybes)

runRobot :: [String] -> [String]
runRobot input = do
  command <- catMaybes $ fmap parseCommand input
  ["Woah"]

startingBoard :: Board
startingBoard = Board 5 5 Nothing
