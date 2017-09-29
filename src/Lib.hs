module Lib (runRobot) where

import           BoardProcessor
import           Parser
import           Types

import           Control.Monad.State  (evalStateT)
import           Control.Monad.Writer (execWriter)
import           Data.Maybe           (catMaybes)

runRobot :: [String] -> [String]
runRobot input =
  let commands = catMaybes $ fmap parseCommand input
      actions = fmap getAction commands
      finalAction = foldl1 (>>) actions
   in execWriter $ evalStateT finalAction startingBoard

startingBoard :: Board
startingBoard = Board (Coordinate 5 5) Nothing
