module BoardProcessor where

import qualified Types as T

import Control.Lens (over, set, _Just)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

execute :: T.Command -> T.Board -> (Maybe String, T.Board)
execute command board = (message, finalBoard)
  where
    newBoard = updateBoard command board
    finalBoard = if validateBoard newBoard then newBoard else board
    message = produceMessage command finalBoard

updateBoard :: T.Command -> T.Board -> T.Board
updateBoard (T.Place x y facing) = set T.boardRobot (Just $ T.Robot x y facing)
updateBoard T.Right = over (placedRobot . T.robotFacing) (adjustDirection 1)
updateBoard T.Left  = over (placedRobot. T.robotFacing) (adjustDirection (-1))
updateBoard T.Move  = over placedRobot move
updateBoard _       = id

placedRobot = T.boardRobot . _Just

produceMessage :: T.Command -> T.Board -> Maybe String
produceMessage T.Report board = report board
produceMessage _  _           = Nothing

directions :: [T.Direction]
directions = [T.North, T.East, T.South, T.West]

getIndex :: T.Direction -> Int
getIndex = fromJust . flip elemIndex directions

adjustDirection :: Int -> T.Direction -> T.Direction
adjustDirection num = (!!) directions . flip mod 4 . fromIntegral . (+ num) . getIndex

move :: T.Robot -> T.Robot
move r@(T.Robot _ _ T.North) = over T.robotY (+1) r
move r@(T.Robot _ _ T.East)  = over T.robotX (+1) r
move r@(T.Robot _ _ T.South) = over T.robotY (flip (-) 1) r
move r@(T.Robot _ _ T.West)  = over T.robotX (flip (-) 1) r

report :: T.Board -> Maybe String
report (T.Board _ _ Nothing) = Nothing
report (T.Board _ _ (Just (T.Robot robotX robotY facing))) =
  Just $ show robotX ++ "," ++ show robotY ++ "," ++ show facing

validateBoard :: T.Board -> Bool
validateBoard (T.Board _ _ Nothing) = True
validateBoard (T.Board boardX boardY (Just (T.Robot robotX robotY _))) =
  robotX > 0 && robotX <= boardX && robotY > 0 && robotY <= boardY
