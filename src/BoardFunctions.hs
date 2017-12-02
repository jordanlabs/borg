module BoardFunctions
  ( place
  , left
  , right
  , move
  , validate
  , report
  ) where

import qualified Types        as T

import           Control.Lens (over, set)
import           Data.Char    (toUpper)
import           Data.List    (elemIndex)
import           Data.Maybe   (fromJust)

directions :: [T.Direction]
directions = [T.North, T.East, T.South, T.West]

getIndex :: T.Direction -> Int
getIndex = fromJust . flip elemIndex directions

adjustDirection :: Int -> T.Direction -> T.Direction
adjustDirection num = (!!) directions . flip mod 4 . (+ num) . getIndex

place :: T.Robot -> T.Board -> T.Board
place = set T.boardRobot . Just

left :: T.Direction -> T.Direction
left = adjustDirection (-1)

right :: T.Direction -> T.Direction
right = adjustDirection 1

move :: T.Robot -> T.Robot
move r@(T.Robot _ T.North) = over (T.robotPosition . T.coordinateY) (+1) r
move r@(T.Robot _ T.East)  = over (T.robotPosition . T.coordinateX) (+1) r
move r@(T.Robot _ T.South) = over (T.robotPosition . T.coordinateY) (flip (-) 1) r
move r@(T.Robot _ T.West)  = over (T.robotPosition . T.coordinateX) (flip (-) 1) r

report :: T.Robot -> String
report (T.Robot (T.Coordinate robotX robotY) facing) =
  show robotX ++ "," ++ show robotY ++ "," ++ fmap toUpper (show facing)

validate :: T.Board -> Bool
validate (T.Board _ Nothing) = True
validate (T.Board (T.Coordinate boardX boardY) (Just (T.Robot (T.Coordinate robotX robotY) _))) =
  robotX > 0 && robotX <= boardX && robotY > 0 && robotY <= boardY
