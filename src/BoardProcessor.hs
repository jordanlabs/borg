{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module BoardProcessor (getAction) where

import qualified Types                as T

import           BoardFunctions       (left, move, right)
import           Control.Lens         (use, (%=), (.=), _Just)
import           Control.Monad.State  (MonadState, StateT)
import           Control.Monad.Writer (MonadWriter, Writer, tell)

getAction :: T.Command -> GameApp ()
getAction (T.Place coords facing) = placeAction coords facing
getAction T.Left                  = leftAction
getAction T.Right                 = rightAction
getAction T.Move                  = moveAction
getAction T.Report                = reportAction

placedRobot = T.boardRobot . _Just
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp = StateT T.Board (Writer [String])

type GameAction = MonadState T.Board

placeAction :: GameAction m => T.Coordinate -> T.Direction -> m ()
placeAction coords facing = T.boardRobot .= Just (T.Robot coords facing)

moveAction :: GameAction m => m ()
moveAction = placedRobot %= move

leftAction :: GameAction m => m ()
leftAction = placedRobotFacing %= left

rightAction :: GameAction m => m ()
rightAction = placedRobotFacing %= right

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  case r of
    (Just r) -> tell [show r]
    _        -> return ()
