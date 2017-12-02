module Lib
  ( runRobot
  ) where

import           Control.Monad.State  (evalStateT)
import           Control.Monad.Writer (execWriter)
import           Data.Maybe           (catMaybes)

import           BoardProcessor       (getAction)
import           Parser               (parseCommand)
import           Types                (Board (Board), Coordinate (Coordinate))

runRobot :: [String] -> [String]
runRobot input =
  let commands = catMaybes $ fmap parseCommand input
      action   = foldMap getAction commands
  in  execWriter $ evalStateT action startingBoard

startingBoard :: Board
startingBoard = Board (Coordinate 5 5) Nothing
