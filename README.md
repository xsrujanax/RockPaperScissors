# Rock Paper Scissors Tournament Simulation

This project is a multithreaded simulation of a Rock, Paper, Scissors tournament. Players compete against each other until a winner is determined based on the credits they have. The simulation demonstrates concurrency, randomization, and game logic in Java.

## Features
- **Multithreaded Simulation**: Each player runs in a separate thread.
- **Dynamic Opponent Selection**: Players randomly select opponents who are still qualified.
- **Tournament Mechanics**: Players lose credits after losing a match, and a player with zero credits is disqualified.
- **Automatic Tie Handling**: If two players tie, they play again until a winner is determined.
- **Comprehensive Reporting**: At the end of the tournament, a report is generated summarizing player performance and the winner.

## File Structure
- `Game.java`: Main game logic, player interactions, and tournament execution.
- `Player.java`: Player properties and behavior, including credits and moves.
- Supporting files (`game.erl`, `player.erl`): Erlang implementations for comparison or alternative execution.

## Prerequisites
1. Java 11 or higher installed on your system.
2. Basic understanding of Java multithreading.

## Instructions to Execute (Java Implementation)

1. **Prepare Input File**:
   - Create a text file (e.g., `players.txt`) with the player data in the following format:
     ```
     {Player1, 5}.
     {Player2, 4}.
     {Player3, 6}.
     ```
   - Each line should contain the player's name and initial credits enclosed in curly braces `{}` and end with a period `.`.

2. **Compile the Code**:
   - Open a terminal/command prompt.
   - Navigate to the directory containing `Game.java` and `Player.java`.
   - Compile the code:
     ```
     javac Game.java Player.java
     ```

3. **Run the Simulation**:
   - Execute the simulation, providing the player data file as an argument:
     ```
     java Game players.txt
     ```

4. **View Results**:
   - The tournament log will be printed to the console, showing match details, player eliminations, and the winner.

## Instructions to Execute (Erlang Implementation)

1. **Install Erlang**:
   - Download and install Erlang/OTP from [Erlang's official website](https://www.erlang.org/).

2. **Compile Erlang Files**:
   - Open a terminal and navigate to the directory containing the `game.erl` and `player.erl` files.
   - Compile the files using the `erlc` command:
     ```
     erlc game.erl player.erl
     ```

3. **Run the Simulation**:
   - Start the Erlang shell:
     ```
     erl
     ```
   - Load the game module:
     ```
     c(game).
     ```
   - Start the tournament by invoking the appropriate function in the `game` module. For example:
     ```
     game:start("players.txt").
     ```
     Ensure that the player data file follows the same format as described in the Java instructions.

4. **View Results**:
   - The output will be displayed in the Erlang shell, showing match details and the tournament winner.

## Example Output
```plaintext
** Rock, Paper Scissors World Championship **
Starting game log...
+ [1] new game for Player1 -> Player2
$ (1) Player1:rock -> Player2:scissors = Player2 loses [3 credits left]
- [2] new game for Player3 -> Player2
$ (2) Player3:paper -> Player2:rock = Player2 loses [2 credits left]

** Tournament Report **
Players:
Player1: credits used: 1, credits remaining: 4
Player2: credits used: 3, credits remaining: 0
Player3: credits used: 1, credits remaining: 5
-----
Total games: 2
winner: Player3
See you next year...
```

## Notes
- The simulation runs until only one player has remaining credits.
- If no valid players are left for a match, the game gracefully ends.

## Erlang Implementation
If you wish to explore the Erlang versions (`game.erl` and `player.erl`), refer to the provided Erlang files. Execution of these files requires an Erlang environment (e.g., Erlang/OTP).

## License
This project is for educational purposes and demonstrates concepts in Java and Erlang programming.

