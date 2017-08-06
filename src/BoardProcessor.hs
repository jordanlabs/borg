module BoardProcessor where

import qualified Types as T

import Control.Lens (over, set, _Just)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

execute :: T.Command -> T.Board -> T.Board
execute (T.Place x y facing) = set T.boardRobot (Just $ T.Robot x y facing)
execute T.Right = over (placedRobot . T.robotFacing) $ adjustDirection 1
execute T.Left  = over (placedRobot. T.robotFacing) $ adjustDirection (-1)
execute T.Move = over placedRobot move

placedRobot = T.boardRobot . _Just

directions :: [T.Direction]
directions = [T.North, T.East, T.South, T.West]

getIndex :: T.Direction -> Int
getIndex = fromJust . flip elemIndex directions

adjustDirection :: Int -> T.Direction -> T.Direction
adjustDirection num = (!!) directions . flip mod 4 . fromIntegral . (+ num) . getIndex

move :: T.Robot -> T.Robot
move r@(T.Robot _ _ T.North) = over T.robotY (+1) r
move r@(T.Robot _ _ T.East) = over T.robotX (+1) r
move r@(T.Robot _ _ T.South) = over T.robotY (flip (-) 1) r
move r@(T.Robot _ _ T.West) = over T.robotX (flip (-) 1) r
