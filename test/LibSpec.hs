module LibSpec where

import Test.Hspec

import Lib

spec :: Spec
spec =
  describe "runRobot" $ do
    it "sequence 1" $
      let input = ["PLACE 1,1,NORTH", "MOVE", "MOVE", "RIGHT", "REPORT"]
       in runRobot input `shouldBe` ["1,3,EAST"]

    it "sequence 2" $
      let input = ["PLACE 1,1,NORTH", "MOVE", "MOVE", "RIGHT"]
       in runRobot input `shouldBe` []

    it "sequence 3" $
      let input = ["MOVE", "PLACE 4,4,SOUTH", "MOVE", "MOVE", "LEFT", "REPORT"]
       in runRobot input `shouldBe` ["4,2,EAST"]

    it "sequence 4" $
      let input = ["PLACE 1,2,SOUTH", "REPORT", "RIGHT", "MOVE", "REPORT", "MOVE"]
       in runRobot input `shouldBe` ["1,2,SOUTH", "1,2,WEST"]

    it "sequence 5" $
      let input = ["PLACE 1,1,SOUTH", "MOVE", "MOVE", "LEFT", "MOVE", "REPORT"]
       in runRobot input `shouldBe` ["2,1,EAST"]

    it "sequence 6" $
      let input = []
      in runRobot input `shouldBe` []
