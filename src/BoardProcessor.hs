{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module BoardProcessor (getAction, GameApp, GameAction) where

import qualified Types                    as T

import           BoardActions             (left, move, right)
import           Control.Lens             (over, set, use, (%=), (.=), _Just)
import           Control.Monad.State.Lazy
import           Control.Monad.Writer
import           Data.Functor.Identity    (Identity)
import           Data.List                (elemIndex)
import           Data.Maybe               (fromJust)

getAction :: T.Command -> GameApp ()
getAction (T.Place x y facing) = placeAction x y facing
getAction T.Left               = leftAction
getAction T.Right              = rightAction
getAction T.Move               = moveAction
getAction T.Report             = reportAction

placedRobot = T.boardRobot . _Just
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp = StateT T.Board (WriterT [String] Identity)

type GameAction = MonadState T.Board

placeAction :: GameAction m => Int -> Int -> T.Direction -> m ()
placeAction x y facing = T.boardRobot .= Just (T.Robot x y facing)

moveAction :: GameAction m => m ()
moveAction =  placedRobot %= move

leftAction :: GameAction m => m ()
leftAction =  placedRobotFacing %= left

rightAction :: GameAction m => m ()
rightAction = placedRobotFacing %= right

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  tell [show r]

validateBoard :: T.Board -> Bool
validateBoard (T.Board _ _ Nothing) = True
validateBoard (T.Board boardX boardY (Just (T.Robot robotX robotY _))) =
  robotX > 0 && robotX <= boardX && robotY > 0 && robotY <= boardY
