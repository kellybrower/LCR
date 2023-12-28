{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module GameLogic where

import Control.Monad (foldM, replicateM)
import Data.List (find)
import System.Random (randomRIO)
import Types (DiceOutcome (..), Game, Player (..))

-- Initialize the game
initGame :: Int -> Game
initGame numPlayers = replicate numPlayers (Player 1 3)

-- Roll the dice for a player
rollDice :: Player -> IO [DiceOutcome]
rollDice player = mapM (const rollOne) [1 .. numDice]
  where
    numDice = min 3 (chips player) -- Players roll up to 3 dice based on their chip count

    -- Roll a single die and get the outcome
    rollOne :: IO DiceOutcome
    rollOne = do
      roll <- randomRIO (1 :: Int, 6) -- Simulate a six-sided die
      return $ case roll of
        1 -> GoLeft
        2 -> GoRight
        3 -> GoCenter
        _ -> NoEffect

-- Check if the game is over
isGameOver :: Game -> Bool
isGameOver game =
  length (filter hasChips game) <= 1
  where
    hasChips :: Player -> Bool
    hasChips player = chips player > 0

-- Assuming previous definitions of Player, DiceOutcome, Game, rollDice, applyRules
-- A player plays a round
-- playRound :: Game -> Int -> IO Game
-- playRound game playerNumber = do
--   let maybePlayer = find (\p -> playerId p == playerNumber) game
--   case maybePlayer of
--     Just player -> do
--       putStrLn $ "Player " ++ show (playerId player) ++ "'s turn. Press any key to roll the dice."
--       _ <- getLine
--       outcomes <- rollDice player
--       putStrLn $ "Player " ++ show (playerId player) ++ " rolled: " ++ show outcomes
--       let updatedGame = applyRules game outcomes
--       return updatedGame
--     Nothing -> return game -- If the player number is not found, return the game unchanged.
-- Assuming Game is a list of Players
playRound :: Game -> Int -> IO Game
playRound game playerNumber = do
  let player = game !! (playerNumber - 1) -- Get the current player based on playerNumber
  outcomes <- rollDice player
  putStrLn $ "Player " ++ show (playerId player) ++ " rolled: " ++ show outcomes
  let updatedGame = applyRules game outcomes playerNumber
  return updatedGame

-- Implement the applyRules function
applyRules :: Game -> [DiceOutcome] -> Game
applyRules game outcomes = foldl applyOutcome game outcomes
  where
    applyOutcome :: Game -> DiceOutcome -> Game
    applyOutcome g outcome = case outcome of
      GoLeft -> moveChip g (-1)
      GoRight -> moveChip g 1
      GoCenter -> centerChip g
      NoEffect -> g

    -- Function to move a chip to the left (-1) or right (1)
    moveChip :: Game -> Int -> Game
    moveChip g direction =
      let currentPlayer = head g
          nextIndex = (playerId currentPlayer + direction + length g) `mod` length g
          nextPlayer = g !! nextIndex
          updatedCurrentPlayer = currentPlayer {chips = chips currentPlayer - 1}
          updatedNextPlayer = nextPlayer {chips = chips nextPlayer + 1}
       in replacePlayer (replacePlayer g updatedCurrentPlayer) updatedNextPlayer

    -- Function to move a chip to the center
    centerChip :: Game -> Game
    centerChip g =
      let currentPlayer = head g
          updatedCurrentPlayer = currentPlayer {chips = chips currentPlayer - 1}
       in replacePlayer g updatedCurrentPlayer

    -- Function to replace a player in the game list
    replacePlayer :: Game -> Player -> Game
    replacePlayer g p = map (\player -> if playerId player == playerId p then p else player) g
