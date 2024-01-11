{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data DiceOutcome = GoLeft | GoRight | GoCenter | NoEffect
  deriving (Show, Eq, Generic, ToJSON)

data Player = Player {playerId :: Int, chips :: Int}
  deriving (Show, Generic, ToJSON)

type Game = ([Player], Int)
