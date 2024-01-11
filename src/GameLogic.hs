{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module GameLogic where

import Control.Monad (foldM, replicateM)
import Data.List (find)
import System.Random (randomRIO)
import Types (DiceOutcome (..), Game, Player (..))

-- -- Initialize the game
-- initGame :: Int -> Game
-- initGame numPlayers = replicate numPlayers (Player 1 3)
--
--
initGame :: Int -> Game
initGame numPlayers = (players, initialPot)
  where
    players = [Player pid 3 | pid <- [1 .. numPlayers]]
    initialPot = 0

safeGet :: Int -> [Player] -> Maybe Player
safeGet i xs
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

addToPot :: Game -> Int -> Game
addToPot (players, pot) amount = (players, pot + amount)

awardPot :: Game -> Int -> Game
awardPot (players, pot) winnerId =
  (updatedPlayers, 0) -- Reset the pot to 0
  where
    updatedPlayers = map updatePlayerChips players

    updatePlayerChips :: Player -> Player
    updatePlayerChips player
      | playerId player == winnerId = player {chips = chips player + pot}
      | otherwise = player

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

isGameOver :: Game -> Bool
isGameOver (players, pot) =
  let playersWithChips = filter hasChips players
   in if length playersWithChips == 1
        then True -- Game over
        else False
  where
    hasChips :: Player -> Bool
    hasChips player = chips player > 0

-- Function to play a round of the game
playRound :: Game -> Int -> IO Game
playRound game@(players, pot) playerNumber = do
  let maybePlayer = safeGet (playerNumber - 1) players -- Get the current player based on playerNumber
  outcomes <- rollDice maybePlayer
  case maybePlayer of
    Just player -> putStrLn $ "Player " ++ show (playerId player) ++ " rolled: " ++ show outcomes
    Nothing -> putStrLn "Invalid player number."
  let updatedGame = case maybePlayer of
        Just player -> applyRules game outcomes playerNumber
        Nothing -> game -- If the player is invalid, return the game unchanged.
  return updatedGame

-- Implement the applyRules function
applyRules :: Game -> [DiceOutcome] -> Int -> Game
applyRules game outcomes playerNumber = foldr (flip $ applyOutcome playerNumber) game outcomes
  where
    applyOutcome :: Int -> Game -> DiceOutcome -> Game
    applyOutcome pNum g outcome = case outcome of
      GoLeft -> moveChip g pNum (-1)
      GoRight -> moveChip g pNum 1
      GoCenter -> centerChip g pNum
      NoEffect -> g

moveChip :: Game -> Int -> Int -> Game
moveChip (players, pot) currentPlayerIndex direction =
  case safeGet currentPlayerIndex players of
    Just currentPlayer ->
      if chips currentPlayer > 0
        then
          let totalPlayers = length players
              nextIndex = (currentPlayerIndex + direction + totalPlayers) `mod` totalPlayers
              gameWithUpdatedCurrentPlayer = updateList players currentPlayerIndex (updateCurrentPlayer currentPlayer)
              updatedPlayers = maybe gameWithUpdatedCurrentPlayer (updateList gameWithUpdatedCurrentPlayer nextIndex . updateNextPlayer) (safeGet nextIndex players)
           in (updatedPlayers, pot)
        else (players, pot)
    Nothing -> (players, pot)
  where
    updateCurrentPlayer player = player {chips = chips player - 1}
    updateNextPlayer player = player {chips = chips player + 1}
    updateList :: [Player] -> Int -> Player -> [Player]
    updateList xs index newElement =
      take index xs ++ [newElement] ++ drop (index + 1) xs

centerChip :: Game -> Int -> Game
centerChip (players, pot) playerNumber =
  let currentPlayerIndex = playerNumber - 1
   in case safeGet currentPlayerIndex players of
        Just currentPlayer ->
          if chips currentPlayer > 0
            then
              let updatedCurrentPlayer = currentPlayer {chips = chips currentPlayer - 1}
                  updatedPlayers = updateList players currentPlayerIndex updatedCurrentPlayer
                  updatedGame = (updatedPlayers, pot + 1)
               in updatedGame
            else (players, pot)
        Nothing -> (players, pot)
  where
    updateList :: [Player] -> Int -> Player -> [Player]
    updateList xs index newElement =
      take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to replace a player in the game list
replacePlayer :: Game -> Player -> Game
replacePlayer (players, pot) playerToReplace =
  (updatedPlayers, pot)
  where
    updatedPlayers = map replaceIfMatch players

    replaceIfMatch :: Player -> Player
    replaceIfMatch player
      | playerId player == playerId playerToReplace = playerToReplace
      | otherwise = player
