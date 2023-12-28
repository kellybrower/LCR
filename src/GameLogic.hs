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
rollDice :: Maybe Player -> IO [DiceOutcome]
rollDice Nothing = return [] -- If there is no player, return an empty list of outcomes.
rollDice (Just player) = mapM (const rollOne) [1 .. numDice]
  where
    numDice = min 3 (chips player) -- Players roll up to 3 dice based on their chip count.

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

safeGet :: Int -> [a] -> Maybe a
safeGet i xs
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

-- Assuming Game is a list of Players
playRound :: Game -> Int -> IO Game
playRound game playerNumber = do
  let maybePlayer = safeGet (playerNumber - 1) game -- Get the current player based on playerNumber
  outcomes <- rollDice maybePlayer
  case maybePlayer of
    Just player -> putStrLn $ "Player " ++ show (playerId player) ++ " rolled: " ++ show outcomes
    Nothing -> putStrLn $ "Invalid player number."
  let updatedGame = case maybePlayer of
        Just player -> applyRules game outcomes playerNumber
        Nothing -> game -- If the player is invalid, return the game unchanged.
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
      gameWithUpdatedCurrentPlayer = maybe game (updateList game currentPlayerIndex . updateCurrentPlayer) (safeGet currentPlayerIndex game)
      updatedGame = maybe gameWithUpdatedCurrentPlayer (updateList gameWithUpdatedCurrentPlayer nextIndex . updateNextPlayer) (safeGet nextIndex game)
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
      maybePlayer = safeGet currentPlayerIndex game
      updatedGame = maybe game (updateGame currentPlayerIndex) maybePlayer
   in updatedGame
  where
    updateGame :: Int -> Player -> Game
    updateGame index player =
      let updatedPlayer = player {chips = chips player - 1}
       in updateList game index updatedPlayer

    updateList :: [Player] -> Int -> Player -> [Player]
    updateList xs index newElement =
      take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to replace a player in the game list
replacePlayer :: Game -> Player -> Game
replacePlayer g p = map (\player -> if playerId player == playerId p then p else player) g
