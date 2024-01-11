{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import GameLogic (applyRules, initGame, isGameOver, playRound, rollDice)
import Types (DiceOutcome, Game, Player (..))

main :: IO ()
main = do
  putStrLn "Enter the number of players:"
  numPlayers <- readLn
  let initialState = initGame numPlayers
  finalStates <- playGame initialState [] 1 numPlayers

  -- Reverse the list of states to have them in chronological order
  let jsonStates = encode (reverse finalStates)
  B.writeFile "my-game-ui/src/game_log.json" jsonStates
  putStrLn "Game log saved to game_log.json"

playGame :: Game -> [Game] -> Int -> Int -> IO [Game]
playGame game states currentPlayer totalPlayers
  | isGameOver game = return (game : states)
  | otherwise = do
      putStrLn $ "Player " ++ show currentPlayer ++ "'s turn, press any key to roll the dice."
      _ <- getLine
      gameAfterTurn <- playRound game currentPlayer
      let nextPlayer = if currentPlayer < totalPlayers then currentPlayer + 1 else 1
      playGame gameAfterTurn (game : states) nextPlayer totalPlayers

displayWinner :: Game -> IO ()
displayWinner (players, pot) =
  case findWinner players of
    Just winner -> do
      putStrLn $ "The winner is Player " ++ show (playerId winner)
      putStrLn $ "Total chips including pot: " ++ show (chips winner + pot)
    Nothing -> putStrLn "No winner."
  where
    findWinner :: [Player] -> Maybe Player
    findWinner = find (\p -> chips p > 0)
