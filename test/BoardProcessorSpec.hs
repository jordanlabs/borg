module BoardProcessorSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe (fromJust, isNothing)
import Control.Lens
import Control.Lens.Getter

import BoardProcessor
import qualified Types as T

genDirection :: Gen T.Direction
genDirection = Test.QuickCheck.elements [T.North, T.East, T.South, T.West]

genRobot :: Gen T.Robot
genRobot = do
  x <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  y <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  f <- genDirection
  return $ T.Robot x y f

genRobotLessThan :: Int -> Int -> Gen T.Robot
genRobotLessThan x' y' = do
  x <- arbitrary `suchThat` (\a -> a >= 1 && a <= x')
  y <- arbitrary `suchThat` (\a -> a >= 1 && a <= y')
  f <- genDirection
  return $ T.Robot x y f

genBoardWithRobot :: Gen T.Board
genBoardWithRobot = do
  x <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  y <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  r <- genRobotLessThan x y
  return $ T.Board x y (Just r)

genUnplacedBoard :: Gen T.Board
genUnplacedBoard = do
  x <- arbitrary `suchThat` (\a -> a >= 0 && a <= 5)
  y <- arbitrary `suchThat` (\a -> a >= 0 && a <= 5)
  return $ T.Board x y Nothing

genNonPlaceCommand :: Gen T.Command
genNonPlaceCommand = Test.QuickCheck.elements [T.Left, T.Right, T.Move, T.Report]

spec :: Spec
spec =
  describe "read" $ do
    it "Non place commands makes no change with an unplaced board" $ property $
      forAll genNonPlaceCommand $ \c ->
        forAll genUnplacedBoard   $ \b ->
          let (message, board) = execute c b
           in isNothing message && board == b

    it "Move never moves the robot off the board" $ property $
      forAll genBoardWithRobot $ \b ->
        let (_, board) = execute T.Move b
            boardX = view T.boardXSize board
            boardY = view T.boardYSize board
            robotX = fromJust $ board ^. T.boardRobot ^? (_Just . T.robotX)
            robotY = fromJust $ board ^. T.boardRobot ^? (_Just . T.robotY)
         in robotX <= boardX && robotY <= boardY 
