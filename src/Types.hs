{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Types where

data DiceOutcome = GoLeft | GoRight | GoCenter | NoEffect deriving (Show, Eq)

data Player = Player {playerId :: Int, chips :: Int} deriving (Show)

type Game = [Player]
