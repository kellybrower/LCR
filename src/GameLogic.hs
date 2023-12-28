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

-- Assuming Game is a list of Players
playRound :: Game -> Int -> IO Game
playRound game playerNumber = do
  let player = game !! (playerNumber - 1) -- Get the current player based on playerNumber
  outcomes <- rollDice player
  putStrLn $ "Player " ++ show (playerId player) ++ " rolled: " ++ show outcomes
  let updatedGame = applyRules game outcomes playerNumber
  return updatedGame

-- Implement the applyRules function
applyRules :: Game -> [DiceOutcome] -> Int -> Game
applyRules game outcomes playerNumber = foldl (applyOutcome playerNumber) game outcomes
  where
    applyOutcome :: Int -> Game -> DiceOutcome -> Game
    applyOutcome pNum g outcome = case outcome of
      GoLeft -> moveChip g pNum (-1)
      GoRight -> moveChip g pNum 1
      GoCenter -> centerChip g pNum
      NoEffect -> g

moveChip :: Game -> Int -> Int -> Game
moveChip game currentPlayerIndex direction =
  let totalPlayers = length game
      nextIndex = (currentPlayerIndex + direction + totalPlayers) `mod` totalPlayers
      gameWithUpdatedCurrentPlayer = updateList game currentPlayerIndex (updateCurrentPlayer (game !! currentPlayerIndex))
      updatedGame = updateList gameWithUpdatedCurrentPlayer nextIndex (updateNextPlayer (game !! nextIndex))
   in updatedGame
  where
    updateCurrentPlayer player = player {chips = chips player - 1}
    updateNextPlayer player = player {chips = chips player + 1}

    updateList :: [Player] -> Int -> Player -> [Player]
    updateList xs index newElement =
      take index xs ++ [newElement] ++ drop (index + 1) xs

centerChip :: Game -> Int -> Game
centerChip game playerNumber =
  let currentPlayerIndex = playerNumber - 1 -- Adjusting playerNumber to 0-based index
      currentPlayer = game !! currentPlayerIndex
      updatedCurrentPlayer = currentPlayer {chips = chips currentPlayer - 1}
   in updateList game currentPlayerIndex updatedCurrentPlayer
  where
    updateList :: [Player] -> Int -> Player -> [Player]
    updateList xs index newElement =
      take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to replace a player in the game list
replacePlayer :: Game -> Player -> Game
replacePlayer g p = map (\player -> if playerId player == playerId p then p else player) g
