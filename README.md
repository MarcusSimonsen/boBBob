# boBBob
FSharp bot for playing Scrabble

### Running the program
From the bobBob directory run: 

```bash
dotnet run --project ./ScrabbleTemplate/
```

It may take some time before the game begins, as the GADDAG needs to be built

Enable or disable debug prints on line 22 in `Program.fs`

### Achieved
- Finish a game on infinite board
- Multi-player and dictionary
  - To play multiplayer, comment in line 58 in `Program.fs` and comment out line 57 
### Not achieved
- Playing on all boards
- Parallelism
- Respect the timeout flag
