{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (makeLenses)

data Board = Board
  { _maxCoordinate :: Coordinate
  , _boardRobot    :: Maybe Robot }
  deriving (Show, Eq)

data Robot = Robot
  { _robotPosition :: Coordinate
  , _robotFacing   :: Direction }
  deriving (Show, Eq)

data Direction = North | East | South | West
  deriving (Show, Eq)

data Coordinate = Coordinate
  { _coordinateX :: Int
  , _coordinateY :: Int }
  deriving (Show, Eq)

data Command =
    Move
  | Left
  | Right
  | Report
  | Place Coordinate Direction
  deriving (Show, Eq)

makeLenses ''Board
makeLenses ''Robot
makeLenses ''Coordinate
