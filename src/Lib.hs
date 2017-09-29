module Lib (runRobot) where

import           BoardProcessor
import           Parser
import           Types

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Foldable         (foldl')
import           Data.Functor.Identity (runIdentity)
import           Data.Maybe            (catMaybes)

runRobot :: [String] -> [String]
runRobot input =
  let commands = catMaybes $ fmap parseCommand input
      actions = fmap getAction commands
      finalAction = foldl1 (>>) actions
   in runIdentity $ execWriterT $ evalStateT finalAction startingBoard

startingBoard :: Board
startingBoard = Board 5 5 Nothing
