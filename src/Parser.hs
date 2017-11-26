{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parser (parseCommand) where

import qualified Types                  as T

import           Text.Megaparsec        (char, parseMaybe, space, string',
                                         (<|>))
import qualified Text.Megaparsec.Lexer  as L (integer)
import           Text.Megaparsec.String (Parser)

move :: Parser T.Command
move = T.Move <$ string' "MOVE"

left :: Parser T.Command
left = T.Left <$ string' "LEFT"

right :: Parser T.Command
right = T.Right <$ string' "RIGHT"

report :: Parser T.Command
report = T.Report <$ string' "REPORT"

direction :: Parser T.Direction
direction = (T.North <$ string' "NORTH")
        <|> (T.East <$ string' "EAST")
        <|> (T.South <$ string' "SOUTH")
        <|> (T.West <$ string' "WEST")

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
