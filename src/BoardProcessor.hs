{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module BoardProcessor (getAction) where

import qualified Types                as T

import           BoardFunctions       (place, left, move, right, validate)
import           Control.Lens         (use, (%=), (.=), _Just, over, set)
import           Control.Monad.State  (MonadState, StateT, get, put)
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
placeAction coords facing = validatedAction $ place (T.Robot coords facing)

moveAction :: GameAction m => m ()
moveAction = validatedAction (over placedRobot move)

leftAction :: GameAction m => m ()
leftAction = placedRobotFacing %= left

rightAction :: GameAction m => m ()
rightAction = placedRobotFacing %= right

validatedAction :: GameAction m => (T.Board -> T.Board) -> m ()
validatedAction updateBoard = do
  current <- get
  let updated = updateBoard current
      final   = if validate updated then updated else current
  put final

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  case r of
    (Just r) -> tell [show r]
    _        -> return ()
