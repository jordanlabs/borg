{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser
  ( parseCommand
  ) where

import qualified Types                  as T

import           Data.Functor           (($>))
import           Text.Megaparsec        (char, parseMaybe, space, string',
                                         (<|>))
import qualified Text.Megaparsec.Lexer  as L (integer)
import           Text.Megaparsec.String (Parser)

move :: Parser T.Command
move = string' "MOVE" $> T.Move

left :: Parser T.Command
left = string' "LEFT" $> T.Left

right :: Parser T.Command
right = string' "RIGHT" $> T.Right

report :: Parser T.Command
report = string' "REPORT" $> T.Report

direction :: Parser T.Direction
direction = (string' "NORTH" $> T.North)
        <|> (string' "EAST"  $> T.East)
        <|> (string' "SOUTH" $> T.South)
        <|> (string' "WEST"  $> T.West)

place :: Parser T.Command
place = do
  string' "PLACE"
  space
  x <- L.integer
  char ','
  y <- L.integer
  char ','
  d <- direction
  return $ T.Place (T.Coordinate (fromIntegral x) (fromIntegral y)) d

commandParser :: Parser T.Command
commandParser = move <|> left <|> right <|> place <|> report

parseCommand :: String -> Maybe T.Command
parseCommand = parseMaybe commandParser
