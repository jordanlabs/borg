{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens

data Board = Board
  { _boardXSize :: Int
  , _boardYSize :: Int
  , _boardRobot :: Maybe Robot }
  deriving Show

data Robot = Robot
  { _robotX :: Int
  , _robotY :: Int
  , _robotFacing :: Direction }
  deriving Show

data Direction = North | East | South | West
  deriving (Show, Eq)

data Command =
    Move
  | Left
  | Right
  | Report
  | Place Int Int Direction
  deriving Show

makeLenses ''Board
makeLenses ''Robot
