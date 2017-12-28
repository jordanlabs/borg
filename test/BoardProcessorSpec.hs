module BoardProcessorSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.State (execStateT)
import Control.Monad.Writer (runWriter)
-- import Test.QuickCheck (forAll, property)
import Test.QuickCheck.Gen (generate)
import Control.Lens (set, _Just)

import Generators (genBoardNoRobot, genPlaceCommandWithin, genBoardValidRobot)
import BoardProcessor (getAction)
import qualified Types as T

spec :: Spec
spec = describe "BoardProcessor" $ do

  describe "placeAction" $
    it "performs a place" $ do
      board@(T.Board bc@(T.Coordinate x y) _) <- generate genBoardNoRobot
      command@(T.Place cc cdir)               <- generate $ genPlaceCommandWithin x y
      let action        = getAction command
          (result, _)   = runWriter $ execStateT action board
          expected      = T.Board bc (Just $ T.Robot cc cdir)
      result `shouldBe` expected

  describe "leftAction" $
    it "performs a left" $ do
      board@(T.Board bc (Just (T.Robot rc _))) <- generate genBoardValidRobot
      let testBoard     = set (T.boardRobot . _Just . T.robotFacing) T.North board
          action        = getAction T.TurnLeft
          (result, _)   = runWriter $ execStateT action testBoard
          expected      = T.Board bc (Just $ T.Robot rc T.West)
      result `shouldBe` expected

  describe "rightAction" $
    it "performs a right" $ do
      board@(T.Board bc (Just (T.Robot rc _))) <- generate genBoardValidRobot
      let testBoard     = set (T.boardRobot . _Just . T.robotFacing) T.North board
          action        = getAction T.TurnRight
          (result, _)   = runWriter $ execStateT action testBoard
          expected      = T.Board bc (Just $ T.Robot rc T.East)
      result `shouldBe` expected

  describe "moveAction" $
    it "performs a move" $
      let action        = getAction $ T.Move
          board         = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 2) T.North)
          (result, _)   = runWriter $ execStateT action board
          expected      = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 3) T.North)
      in  result `shouldBe` expected
