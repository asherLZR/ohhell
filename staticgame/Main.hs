{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import OhHell
import OhTypes
import EitherIO
import Logs
import safe qualified Player
import Control.Monad
import Control.Exception

players :: [Player]
players =
    [ newPlayer "4" Player.playCard Player.makeBid
    , newPlayer "3" Player.playCard Player.makeBid
    , newPlayer "2" Player.playCard Player.makeBid
    , newPlayer "1" Player.playCard Player.makeBid
    ]

main :: IO ()
main = do
    decks <- mapM (\i -> do
        d <- shuffledDeck
        return $ Deck i d) allHands
    mapM_ clearLog players
    played <- runEitherIO $ playGame players decks
    case played of
        Right (GameResult hr scores places updatedPlayers) -> do
            forM_ hr print
            putStrLn "=============="
            forM_ scores print
            forM_ places print
            forM_ updatedPlayers print
        Left e -> print e
