### Scrabble Solver

This repo contains a solver for a word game "Scrabble".

Here's a short overview of the the functionality:

- The functions for reading string representations of both the board and bonus
board lie in the ScrabbleParse.hs module.

- Game represention and functions for showing game board ('showScrabbleBoard'
being most helpful) reside in the Scrabble.hs module.

- The Dictionary for the game is created using function 'makeDictionary' from
module Dictionary.hs

- The functions for finding the best play available as well as updating the board
with a new play are contained in the module ScrabbleSolver.hs.

- ScrabbleAI.hs contains functions for generating (and showing) plays until
the player runs out of tiles (or no solutions can be found).
