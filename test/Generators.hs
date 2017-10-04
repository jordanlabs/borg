module Generators where

import           Test.QuickCheck

import qualified Types           as T

genDirection :: Gen T.Direction
genDirection = Test.QuickCheck.elements [T.North, T.East, T.South, T.West]

genRobot :: Gen T.Robot
genRobot = do
  c <- genCoordinate
  f <- genDirection
  return $ T.Robot c f

genCoordinate :: Gen T.Coordinate
genCoordinate = do
  x <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  y <- arbitrary `suchThat` (\a -> a >= 1 && a <= 5)
  return $ T.Coordinate x y

genRobotLessThan :: Int -> Int -> Gen T.Robot
genRobotLessThan x' y' = do
  x <- arbitrary `suchThat` (\a -> a >= 1 && a <= x')
  y <- arbitrary `suchThat` (\a -> a >= 1 && a <= y')
  f <- genDirection
  return $ T.Robot (T.Coordinate x y) f

genRobotGreaterThan :: Int -> Int -> Gen T.Robot
genRobotGreaterThan x' y' = do
  x <- arbitrary `suchThat` (> x')
  y <- arbitrary `suchThat` (> y')
  f <- genDirection
  return $ T.Robot (T.Coordinate x y) f

genBoardWithRobot :: Gen T.Board
genBoardWithRobot = do
  c <- genCoordinate
  r <- genRobotLessThan (T._coordinateX c) (T._coordinateY c)
  return $ T.Board c (Just r)

genBoardWithInvalidRobot :: Gen T.Board
genBoardWithInvalidRobot = do
  c <- genCoordinate
  r <- genRobotGreaterThan (T._coordinateX c) (T._coordinateY c)
  return $ T.Board c (Just r)

genUnplacedBoard :: Gen T.Board
genUnplacedBoard = do
  c <- genCoordinate
  return $ T.Board c Nothing

genNonPlaceCommand :: Gen T.Command
genNonPlaceCommand = Test.QuickCheck.elements [T.Left, T.Right, T.Move, T.Report]
