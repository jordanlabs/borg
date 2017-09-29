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
move r@(T.Robot _ _ T.North) = over T.robotY (+1) r
move r@(T.Robot _ _ T.East)  = over T.robotX (+1) r
move r@(T.Robot _ _ T.South) = over T.robotY (flip (-) 1) r
move r@(T.Robot _ _ T.West)  = over T.robotX (flip (-) 1) r

report :: T.Board -> Maybe String
report (T.Board _ _ Nothing) = Nothing
report (T.Board _ _ (Just (T.Robot robotX robotY facing))) =
  Just $ show robotX ++ "," ++ show robotY ++ "," ++ show facing
