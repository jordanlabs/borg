module Generators where

import           Test.QuickCheck (Gen, arbitrary, elements, suchThat)

import qualified Types           as T

genDirection :: Gen T.Direction
genDirection = elements [T.North, T.East, T.South, T.West]

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

genBoardValidRobot :: Gen T.Board
genBoardValidRobot = do
  c <- genCoordinate
  r <- genRobotLessThan (T._coordinateX c) (T._coordinateY c)
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
