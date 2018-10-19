module Csv where

import OhTypes
import OhHell
import Data.List
import Data.Char
import Parser
import Instances

{-
Unfortunately an incomplete implementation of a csv parser for the log file.
Unable to get a string of characters efficiently from a line of the log file.
-}

-- | Read in a file from the string given.
getFile :: IO (String)
getFile = do
    fp <- getLine
    readFile fp

-- | Break the lines of a given csv file.
breakLines :: IO([String])
breakLines = do
    l <- getFile
    pure $ lines l

-- | Ignore the header in the log file and process the game.
gameLogs :: IO([String])
gameLogs = tail <$> breakLines

-- | A parser to pull out time
parseTime :: Parser String
parseTime = thisMany 10 digit

-- | A parser to pull out pos, bid, score, first.
parseNumbers :: Parser String
parseNumbers = betweenCharTok ',' ',' (list digit)

-- | A parser for comma then digit String.
commaDigit :: Parser String
commaDigit = do
    _ <- commaTok
    list digit

-- | A parser for comma then a character turned to String.
commaChar :: Parser String
commaChar = do
    _ <- commaTok
    c <- character
    pure $ c:[]

-- | Parses a trick denoted by double quotes.
commaTrick :: Parser String
commaTrick = do
    _ <- commaTok
    betweenCharTok '\"' '\"' allString

-- | Parser that accepts everything in the given string.
-- | Runs incredibly slowly - list function does heavy recursion - could not figure out why.
allString :: Parser String
allString = list $ noneof "\""

-- | Break a trick down based with comma separation.
trickSeparated :: Parser [String]
trickSeparated = sepby (thisMany 2 character) (is ',')

-- | Parses a single line in the log file.
combinedParser :: Parser [String]
combinedParser = do
    time <- parseTime
    pos <- commaDigit
    bid <- commaDigit
    score <- commaDigit
    first <- commaDigit
    trump <- commaChar
    tricks <- commaTrick
    pure(time:pos:bid:score:first:trump:tricks:[])

-- | Gets the datetime from the string.
getDate :: IO(ParseResult String)
getDate = do
    b <- gameLogs
    s <- head <$> pure b
    pure $ parse parseTime s