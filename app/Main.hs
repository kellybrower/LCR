{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import GameLogic (applyRules, initGame, isGameOver, playRound, rollDice)
import Types (DiceOutcome, Game, Player (..)) -- Import Types if needed

-- Assuming DiceOutcome and Player are defined as before

-- -- Main function
-- main :: IO ()
-- main = do
--   let initialGame = [Types.Player {playerId = 1, chips = 3}, Types.Player {playerId = 2, chips = 3}]
--   putStrLn "Starting game..."
--   updatedGame <- GameLogic.playRound initialGame
--   putStrLn "Updated game state:"
--   print updatedGame

-- main :: IO ()
-- main = do
--   let initialGame = initGame 3
--   finalState <- playUntilOver initialGame
--   print finalState
--   where
--     playUntilOver :: Game -> IO Game
--     playUntilOver g
--       | isGameOver g = return g
--       | otherwise = playRound g >>= playUntilOver
--
main :: IO ()
main = do
  putStrLn "Starting the game with 2 players."
  let game = initGame 2 -- Initialize game with 2 players
  finalState <- playGame game
  print finalState

playGame :: Game -> IO Game
playGame game = do
  if isGameOver game
    then return game
    else do
      putStrLn "Player 1's turn, press any key to roll the dice."
      _ <- getLine
      gameAfterP1 <- playRound game 1
      putStrLn "Player 2's turn, press any key to roll the dice."
      _ <- getLine
      gameAfterP2 <- playRound gameAfterP1 2
      playGame gameAfterP2
