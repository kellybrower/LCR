// Assuming this file is src/types/GameTypes.ts

export interface Player {
  playerId: number;
  chips: number;
}

export interface GameState {
  players: Player[];
  pot: number;
}
