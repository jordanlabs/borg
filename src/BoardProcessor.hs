{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BoardProcessor
  ( getAction
  , GameApp
  ) where

import qualified Types                 as T

import           BoardFunctions        (left, move, place, report, right,
                                        validate)
import           Control.Lens          (over, use, (%=), _Just)
import           Control.Monad.State   (MonadState, StateT (StateT), get, put)
import           Control.Monad.Writer  (MonadWriter, Writer, WriterT (WriterT),
                                        tell)
import           Data.Functor.Identity (Identity (Identity))

getAction :: T.Command -> GameApp T.Board
getAction (T.Place coords facing) = placeAction coords facing
getAction T.Left                  = leftAction
getAction T.Right                 = rightAction
getAction T.Move                  = moveAction
getAction T.Report                = reportAction

placedRobot :: (T.Robot -> Identity T.Robot) -> T.Board -> Identity T.Board
placedRobot = T.boardRobot . _Just

placedRobotFacing :: (T.Direction -> Identity T.Direction) -> T.Board -> Identity T.Board
placedRobotFacing = placedRobot . T.robotFacing

type MessageWriter = MonadWriter [String]

type GameApp s = StateT s (Writer [String]) ()

type GameAction = MonadState T.Board

instance Monoid (GameApp s) where
  mempty = StateT $ \s -> WriterT $ Identity (((), s), [])
  mappend = (>>)

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
      final   = if validate updated
                  then updated
                  else current
  put final

reportAction :: (GameAction m, MessageWriter m) => m ()
reportAction = do
  r <- use T.boardRobot
  case r of
    (Just r') -> tell [report r']
    _         -> return ()
