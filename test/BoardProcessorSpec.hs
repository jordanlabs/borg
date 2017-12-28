module BoardProcessorSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.State (execStateT)
import Control.Monad.Writer (runWriter)
-- import Test.QuickCheck (forAll, property)
import Test.QuickCheck.Gen (generate)

import Generators (genBoardNoRobot, genPlaceCommandWithin)
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
    it "performs a left" $
      let action        = getAction $ T.TurnLeft
          board         = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 2) T.North)
          (result, _)   = runWriter $ execStateT action board
          expected      = T.Board (T.Coordinate 5 5) (Just $ T.Robot (T.Coordinate 2 2) T.West)
      in  result `shouldBe` expected
