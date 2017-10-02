{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens (makeLenses)
import           Data.Char    (toUpper)

data Board = Board
  { _maxCoordinate :: Coordinate
  , _boardRobot    :: Maybe Robot }
  deriving (Eq, Show)

instance Show Robot where
  show (Robot (Coordinate robotX robotY) facing) =
    show robotX ++ "," ++ show robotY ++ "," ++ fmap toUpper (show facing)

data Robot = Robot
  { _robotPosition :: Coordinate
  , _robotFacing   :: Direction }
  deriving Eq

data Direction = North | East | South | West
  deriving (Eq, Show)

data Coordinate = Coordinate
  { _coordinateX :: Int
  , _coordinateY :: Int }
  deriving (Eq, Show)

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
