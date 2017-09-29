module Parser (parseCommand) where

import qualified Types                  as T

import           Text.Megaparsec        (char, parseMaybe, space, string',
                                         (<|>))
import qualified Text.Megaparsec.Lexer  as L (integer)
import           Text.Megaparsec.String (Parser)

move :: Parser T.Command
move = string' "MOVE" >> return T.Move

left :: Parser T.Command
left = string' "LEFT" >> return T.Left

right :: Parser T.Command
right = string' "RIGHT" >> return T.Right

report :: Parser T.Command
report = string' "REPORT" >> return T.Report

direction :: Parser T.Direction
direction = (string' "NORTH" >> return T.North)
        <|> (string' "EAST" >> return T.East)
        <|> (string' "SOUTH" >> return T.South)
        <|> (string' "WEST" >> return T.West)

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
