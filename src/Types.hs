module Types where

data Board = Board Int Int (Maybe Robot)
  deriving Show

data Robot = Robot Int Int Direction
  deriving Show

data Direction = North | East | South | West
  deriving Show

data Command =
    Move
  | Left
  | Right
  | Report
  | Place Int Int Direction
  deriving Show
