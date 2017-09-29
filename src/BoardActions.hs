{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module BoardActions (left, right, move, report) where

import qualified Types        as T

import           Control.Lens (over)
import           Data.List    (elemIndex)
import           Data.Maybe   (fromJust)

directions :: [T.Direction]
directions = [T.North, T.East, T.South, T.West]

getIndex :: T.Direction -> Int
getIndex = fromJust . flip elemIndex directions

adjustDirection :: Int -> T.Direction -> T.Direction
adjustDirection num = (!!) directions . flip mod 4 . fromIntegral . (+ num) . getIndex

left :: T.Direction -> T.Direction
left = adjustDirection (-1)

right :: T.Direction -> T.Direction
right = adjustDirection 1

move :: T.Robot -> T.Robot
move r@(T.Robot _ T.North) = over (T.robotPosition . T.coordinateY) (+1) r
move r@(T.Robot _ T.East)  = over (T.robotPosition . T.coordinateX) (+1) r
move r@(T.Robot _ T.South) = over (T.robotPosition . T.coordinateY) (flip (-) 1) r
move r@(T.Robot _ T.West)  = over (T.robotPosition . T.coordinateX) (flip (-) 1) r

validateBoard :: T.Board -> Bool
validateBoard (T.Board _ Nothing) = True
validateBoard (T.Board (T.Coordinate boardX boardY) (Just (T.Robot (T.Coordinate robotX robotY) _))) =
  robotX > 0 && robotX <= boardX && robotY > 0 && robotY <= boardY

report :: T.Board -> Maybe String
report (T.Board _ Nothing) = Nothing
report (T.Board _ (Just (T.Robot (T.Coordinate robotX robotY) facing))) =
  Just $ show robotX ++ "," ++ show robotY ++ "," ++ show facing
