module ParserSpec where

import           Test.Hspec

import           Parser
import qualified Types      as T

spec :: Spec
spec =
  describe "Parser.parseCommand" $ do
    it "left command" $
      parseCommand "LEFT" `shouldBe` Just T.Left

    it "right command" $
      parseCommand "RIGHT" `shouldBe` Just T.Right

    it "move command" $
      parseCommand "MOVE" `shouldBe` Just T.Move

    it "report command" $
      parseCommand "REPORT" `shouldBe` Just T.Report

    it "place command" $
      parseCommand "PLACE 1,1,North" `shouldBe` Just (T.Place (T.Coordinate 1 1) T.North)

    it "mixed case command" $
      parseCommand "LeFt" `shouldBe` Just T.Left

    it "other command" $
      parseCommand "JUMP" `shouldBe` Nothing
