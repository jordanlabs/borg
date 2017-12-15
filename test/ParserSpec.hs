module ParserSpec where

import           Test.Hspec (Spec, describe, it, shouldBe)

import           Parser     (parseCommand)
import qualified Types      as T

spec :: Spec
spec =
  describe "Parser.parseCommand" $ do
    it "left command" $
      parseCommand "LEFT" `shouldBe` Just T.TurnLeft

    it "right command" $
      parseCommand "RIGHT" `shouldBe` Just T.TurnRight

    it "move command" $
      parseCommand "MOVE" `shouldBe` Just T.Move

    it "report command" $
      parseCommand "REPORT" `shouldBe` Just T.Report

    it "place command" $
      parseCommand "PLACE 1,1,North" `shouldBe` Just (T.Place (T.Coordinate 1 1) T.North)

    it "mixed case command" $
      parseCommand "LeFt" `shouldBe` Just T.TurnLeft

    it "other command" $
      parseCommand "JUMP" `shouldBe` Nothing
