import React, { useState, useEffect } from 'react';
import GameBoard from './components/GameBoard';
import { GameState, Player } from './types/GameTypes';
import './App.css';

function App() {
  const [gameStates, setGameStates] = useState<GameState[]>([]);
  const [currentIndex, setCurrentIndex] = useState<number>(0);

  useEffect(() => {
    fetch('game_log.json')
      .then(response => response.json())
      .then(data => {
        // Assuming the data is an array of arrays with the first element as players and the second as pot
        const formattedData = data.map(([players, pot]: [Player[], number]) => {
          return { players, pot };
        });
        setGameStates(formattedData);
        console.log('Data loaded:', formattedData); // Log to confirm the data is loaded and formatted
      })
      .catch(error => {
        console.error('Error fetching game data:', error);
      });
  }, []);

  const handleNextClick = () => {
    setCurrentIndex((prevIndex) => {
      const nextIndex = prevIndex < gameStates.length - 1 ? prevIndex + 1 : prevIndex;
      console.log(`Moving to next index: ${nextIndex}`);
      return nextIndex;
    });
  };

  const handlePreviousClick = () => {
    setCurrentIndex((prevIndex) => {
      const newIndex = prevIndex > 0 ? prevIndex - 1 : 0;
      console.log(`Moving to previous index: ${newIndex}`);
      return newIndex;
    });
  };

  const currentGameState = gameStates[currentIndex];

  return (
    <div className="App">
      <header className="App-header">
        <h1>LCR Game</h1>
        {gameStates.length > 0 ? (
          <>
            <h2>Round {currentIndex + 1}</h2>
            <GameBoard players={currentGameState.players} pot={currentGameState.pot} />
            <button onClick={handlePreviousClick} disabled={currentIndex === 0}>
              Previous State
            </button>
            <button onClick={handleNextClick} disabled={currentIndex === gameStates.length - 1}>
              Next State
            </button>
          </>
        ) : (
          <p>Loading game data...</p>
        )}
      </header>
    </div>
  );
}

export default App;
