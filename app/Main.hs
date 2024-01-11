{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.List (find)
import GameLogic (applyRules, initGame, isGameOver, playRound, rollDice)
import Types (DiceOutcome, Game, Player (..))

main :: IO ()
main = do
  putStrLn "Enter the number of players:"
  numPlayers <- readLn
  let game = initGame numPlayers
  finalState <- playGame game 1 numPlayers
  displayWinner finalState

playGame :: Game -> Int -> Int -> IO Game
playGame game currentPlayer totalPlayers
  | isGameOver game = return game
  | otherwise = do
      putStrLn $ "Player " ++ show currentPlayer ++ "'s turn, press any key to roll the dice."
      _ <- getLine
      gameAfterTurn <- playRound game currentPlayer
      let nextPlayer = if currentPlayer < totalPlayers then currentPlayer + 1 else 1
      playGame gameAfterTurn nextPlayer totalPlayers

displayWinner :: Game -> IO ()
displayWinner (players, pot) =
  case findWinner players of
    Just winner -> do
      putStrLn $ "The winner is Player " ++ show (playerId winner)
      putStrLn $ "Total chips including pot: " ++ show (chips winner + pot)
    Nothing -> putStrLn "No winner."
  where
    findWinner :: [Player] -> Maybe Player
    findWinner ps = find (\p -> chips p > 0) ps
