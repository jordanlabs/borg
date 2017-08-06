module BoardProcessor where

import qualified Types as T

import Control.Lens (over, set)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

execute :: T.Command -> T.Board -> T.Board
execute (T.Place x y facing) = set T.boardRobot (Just $ T.Robot x y facing)
execute T.Right = over T.boardRobot $ fmap (adjustDirection 1)
execute T.Left  = over T.boardRobot $ fmap (adjustDirection (-1))

directions :: [T.Direction]
directions = [T.North, T.East, T.South, T.West]

getIndex :: T.Direction -> Int
getIndex = fromJust . flip elemIndex directions

adjustDirection :: Int -> T.Robot -> T.Robot
adjustDirection = over T.robotFacing . newIndex
  where
    newIndex num = (!!) directions . flip mod 4 . fromIntegral . (+ num) . getIndex
