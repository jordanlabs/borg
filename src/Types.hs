{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens (makeLenses)

data Board = Board
  { _maxCoordinate :: Coordinate
  , _boardRobot    :: Maybe Robot
  } deriving (Eq, Show)

data Robot = Robot
  { _robotPosition :: Coordinate
  , _robotFacing   :: Direction
  } deriving (Eq, Show)

data Direction =
    North
  | East
  | South
  | West
  deriving (Eq, Show)

data Coordinate = Coordinate
  { _coordinateX :: Int
  , _coordinateY :: Int
  } deriving (Eq, Show)

data Command =
    Move
  | Left
  | Right
  | Report
  | Place Coordinate Direction
  deriving (Eq, Show)

makeLenses ''Board
makeLenses ''Robot
makeLenses ''Coordinate
