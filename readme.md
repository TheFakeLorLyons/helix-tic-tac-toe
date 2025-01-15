## __Project Description__

Goal: Improve upon my previous Tic-Tac-Toe console application using the helix library for CLJS.

This project is designed to demonstrate the implementation of Tic-Tac-Toe as a cljs web application with features such as Player vs. Computer and Player vs Player modes, stat tracking, and theme toggles.

### __MVP Goals__
  
The minimum viable product (MVP) for this project includes the following features:

- Take a users name as input, and hold it in state throughout all the games that a player decides to continue playing.

- Basic move validation: Ensure that players can only place their marks in unoccupied spaces. Player entry may only be integers between 0 and 2. [0 1 2]

- Win condition checks: The game ends when either player wins or when the board is full (draw). In this case the computer has been optimized to win or draw every game.

- Browser Interface (mobile friendly)

- Game replay option: After each game, players can choose to play again. Statistics for the matches are saved in local storage and the user is provided a running tally of their current statistics.

- Player vs. Player in addition to Player vs. Computer.

- A button to switch between themes: a plain background, or an svg picture theme. The svg theme is synced to the gameplay.
  
### __Installation__

1. Ensure you have Clojure installed on your machine. You can follow the official installation guide.

2. Clone this repository to your local machine:

3. git clone https://github.com/TheFakeLorLyons/helix-tic-tac-toe.git

4. cd root

5. npm install in the terminal

6. npm start will begin shadow-cljs and load the app in the browser at localhost:8080

#### __...alternatively, enjoy the game in the browser!__

The game is hosted at https://TheFakeLorLyons.github.io/helix-tic-tac-toe via github pages

### __Technologies Used__

- Clojurescript: The programming language used to build this application. In this case I used helix, which is meant to emulate
react as much as possible. I ended up using refx with helix for state management because it made any changes to state more clearly
separated from the component logic. I also used SVGs a lot in this, although there is only one included in the final project.

- Calva/WSL/VS Code: My actual coding setup and helpful packages.

- Git,flowstorm debugger: supporting technologies! Helping me to host this and also ensure it is working correctly respectively.

- Smart git and github pages of course to host this

#### __Other Potential Future Improvements__

- Multiplayer: Extend the game to support network play where players can play from different machines.

- Multidimensionality: Level the game up by doing cubes or other geometric figures.

- More themes and backgrounds.

###### Thank you for your time and consideration! 1/14/2025 -Lor
🐍