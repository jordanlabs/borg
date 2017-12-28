module Generators where

import           Test.QuickCheck     (Gen, elements)
import           Test.QuickCheck.Gen (choose)

import qualified Types               as T

genDirection :: Gen T.Direction
genDirection = elements [T.North, T.East, T.South, T.West]

genRobot :: Gen T.Robot
genRobot = do
  c <- genCoordinate
  f <- genDirection
  return $ T.Robot c f

genCoordinate :: Gen T.Coordinate
genCoordinate = do
  x <- choose (1, 5)
  y <- choose (1, 5)
  return $ T.Coordinate x y

genRobotWithin :: Int -> Int -> Gen T.Robot
genRobotWithin x y = do
  x' <- choose (1, x)
  y' <- choose (1, y)
  f  <- genDirection
  return $ T.Robot (T.Coordinate x' y') f

genRobotGreaterThan :: Int -> Int -> Gen T.Robot
genRobotGreaterThan x y = do
  x' <- choose (x + 1, maxBound)
  y' <- choose (y + 1, maxBound)
  f  <- genDirection
  return $ T.Robot (T.Coordinate x' y') f

genBoardValidRobot :: Gen T.Board
genBoardValidRobot = do
  c <- genCoordinate
  r <- genRobotWithin (T._coordinateX c) (T._coordinateY c)
  return $ T.Board c (Just r)

genBoardInvalidRobot :: Gen T.Board
genBoardInvalidRobot = do
  c <- genCoordinate
  r <- genRobotGreaterThan (T._coordinateX c) (T._coordinateY c)
  return $ T.Board c (Just r)

genBoardNoRobot :: Gen T.Board
genBoardNoRobot = do
  c <- genCoordinate
  return $ T.Board c Nothing

genBoardAndValidPlace :: Gen (T.Board, T.Robot)
genBoardAndValidPlace = do
  b@(T.Board (T.Coordinate x y) _) <- genBoardNoRobot
  r                                <- genRobotWithin x y
  return (b, r)

genPlaceCommandWithin :: Int -> Int -> Gen T.Command
genPlaceCommandWithin x y = do
  x' <- choose (1, x)
  y' <- choose (1, y)
  d  <- genDirection
  return $ T.Place (T.Coordinate x' y') d

