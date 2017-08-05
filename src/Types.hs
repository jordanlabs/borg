module Types where

data Direction = North | East | South | West
  deriving Show

data Command =
    Move
  | Left
  | Right
  | Report
  | Place Int Int Direction
  deriving Show
