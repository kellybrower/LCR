// src/components/GameBoard.tsx

import React from 'react';
import { Player } from '../types/GameTypes';

interface GameBoardProps {
  players: Player[] | undefined;
  pot: number | undefined;
}

const GameBoard: React.FC<GameBoardProps> = ({ players, pot }) => {
  if (!players || typeof pot !== 'number') {
    return <div>Loading...</div>;
  }

  return (
    <div className="GameBoard">
      <div className="Pot">Pot: {pot}</div>
      {players.map(player => (
        <div key={player.playerId} className="Player">
          Player {player.playerId}: {player.chips} chips
        </div>
      ))}
    </div>
  );
};

export default GameBoard;
