-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Parser
import qualified Types as T

main :: IO ()
main = hspec $
  describe "Parser.parseCommand" $
    do
      it "left command" $
        parseCommand "LEFT" `shouldBe` Just T.Left

      it "right command" $
        parseCommand "RIGHT" `shouldBe` Just T.Right

      it "move command" $
        parseCommand "MOVE" `shouldBe` Just T.Move

      it "report command" $
        parseCommand "REPORT" `shouldBe` Just T.Report

      it "place command" $
        parseCommand "PLACE 1,1,North" `shouldBe` Just (T.Place 1 1 T.North)

      it "mixed case command" $
        parseCommand "LeFt" `shouldBe` Just T.Left

      it "other command" $
        parseCommand "JUMP" `shouldBe` Nothing
