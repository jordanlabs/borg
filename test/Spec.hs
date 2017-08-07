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
      it "parsing a left command" $
        parseCommand "LEFT" `shouldBe` Just T.Left

      it "parsing a right command" $
        parseCommand "RIGHT" `shouldBe` Just T.Right

      it "parsing a move command" $
        parseCommand "MOVE" `shouldBe` Just T.Move

      it "parsing a REPORT command" $
        parseCommand "REPORT" `shouldBe` Just T.Report

      it "parsing a place command" $
        parseCommand "PLACE 1,1,North" `shouldBe` Just (T.Place 1 1 T.North)
