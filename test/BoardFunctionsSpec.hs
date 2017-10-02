module BoardFunctionsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Maybe (fromJust, isNothing)
import Control.Lens
import Control.Lens.Getter

import BoardFunctions
import qualified Types as T

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

genBoardWithRobot :: Gen T.Board
genBoardWithRobot = do
  c <- genCoordinate
  r <- genRobotLessThan (T._coordinateX c) (T._coordinateY c)
  return $ T.Board c (Just r)

genUnplacedBoard :: Gen T.Board
genUnplacedBoard = do
  c <- genCoordinate
  return $ T.Board c Nothing

genNonPlaceCommand :: Gen T.Command
genNonPlaceCommand = Test.QuickCheck.elements [T.Left, T.Right, T.Move, T.Report]

spec :: Spec
spec = describe "BoardFunctions" $ do

  describe "place" $
    it "places a robot on the board" $ property $
      forAll genRobot $ \r ->
        place r (T.Board (T.Coordinate 5 5) Nothing) `shouldBe` T.Board (T.Coordinate 5 5) (Just r)

  describe "move" $ do
    it "moves north" $
      let starting = T.Robot (T.Coordinate 1 1) T.North
          expected = T.Robot (T.Coordinate 1 2) T.North
       in move starting `shouldBe` expected

    it "moves east" $
      let starting = T.Robot (T.Coordinate 1 1) T.East
          expected = T.Robot (T.Coordinate 2 1) T.East
       in move starting `shouldBe` expected

    it "moves south" $
      let starting = T.Robot (T.Coordinate 2 2) T.South
          expected = T.Robot (T.Coordinate 2 1) T.South
       in move starting `shouldBe` expected

    it "moves west" $
      let starting = T.Robot (T.Coordinate 2 2) T.West
          expected = T.Robot (T.Coordinate 1 2) T.West
       in move starting `shouldBe` expected

  describe "right" $ do
    it "turns right from north" $
      right T.North `shouldBe` T.East

    it "turns right from east" $
      right T.East `shouldBe` T.South

    it "turns right from south" $
      right T.South `shouldBe` T.West

    it "turns right from west" $
      right T.West `shouldBe` T.North

  describe "left" $ do
    it "turns left from north" $
      left T.North `shouldBe` T.West

    it "turns left from east" $
      left T.West `shouldBe` T.South

    it "turns left from south" $
      left T.South `shouldBe` T.East

    it "turns left from west" $
      left T.East `shouldBe` T.North
