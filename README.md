# Hex Log

I'm going to try and implement the minmax algorithm in a functional programming language called Haskell. It'll be a bit of a personal challenge more than anything else to test if I have understood the content and method of the minmax algorithm. Even though Haskell is a cross platform language, I'll be sure to compile it in an exe and export it to save some time.

I've decided to make the game 'Hex'. It looks like a mixture of Tic-Tac-Toe and Connect Four: https://en.wikipedia.org/wiki/Hex_(board_game)

Firstly, I'm going to get some input/output going and simply try to interpret what the user types as coordinates. I'm going for letters representing columns (x) and integers representing rows (y). The logic will be located in `Main.hs` and my structures will be in `Types.hs`. I recommend starting in `Types.hs` just to see the architecture of the game and how things fit together. There is a data type called `Player` which has two constructors, either `P1` or `P2`. If it's unclear on what this means, it means that `Player` acts like a `Bool` in the sense that it can take two forms; `P1` or `P2`. While our minmax algorithm will interpret `P1` as min and `P2` as max, the program will simply use these as a record of who's turn it is.

The basics of the game are now implemented. The function `tryMakeMove` takes the game state along with the location of the next move and will try to produce a new `GameState` if the move was valid. The `GameState` encapsulates who's turn it is next and will make the move accordingly.

Next, I've implemented the drawing of the board. It is colourful and has annotations on the boarders to help with letter placement. This scales for any size, although seeing as my input takes two characters, the number `10` would be invalid as I can only parse one character for the row number. While this is a flaw, I wouldn't want to go over a `10x10` grid anyway.

I've just implemented the win conditions. Two functions have been placed - `checkWin` and `checkWinChain` where `checkWin` is a simple function and starts the process, `checkWinChain` is called by `checkWin` as a recursive method of finding wins. The key part to this function is the following:

```hs
checkWinChain :: [((Int, Int), Tile)] -> Player -> [(Int, Int)] -> (Int, Int) -> Bool
checkWinChain grid p history search@(x, y)
  | lookup search grid /= Just (Stone p) = False
  | goal search = True
  | otherwise = foldl (||) False $ map check searches'
```
If the current tile we're examining does *not* contain a stone aligned to the current player, return false. If it is, then check if it meets the requirements of a 'goal' - `P1`'s goals are the on the right side of the board seeing as this search starts from the left side. If it's not a goal, then we recursively search all the tiles around the current one.

The line `foldl (||) False $ map check searches` is the important part, `map check searches'` converts each search into a boolean value, which is then `or`ed using `(||)` to collapse the list of bools into a singular bool. Since `True || anything` yields `True`, and because Haskell is a lazy language and things aren't evaluated until they are needed, execution of the recursion only continues while this overarching boolean is `False`, and as soon as it is `True` the rest of it no longer requires evaluation. With this, the player can win the game.

I've created a function called `handleControlType` which changes the method of gathering the location of the next move depending on the control type - `Manual` allows the player to input their moves while `Random` will just randomly select a location out of a list of eligible possibilities.

Next up is the minmax algorithm. Everything else is in place and, just like the random inputs, I need to create a method of deciding the next marker location using the minmax algorithm. In `Types.hs`, I've created the structure `MinMaxNode`. While Haskell has many Tree structures ready to use, I made my own just to have more control. I was originally going to use `GameState`s as my nodes, but I'm creating a new one so that the only data in the structure is the data `MinMaxNode` needs.

I've cleaned up the minmax code a little bit into one function: `constructNode`. This function essentially takes parameters from the current state and recursively sets up a tree up to the desired depth. This tree assumes that the other player will play their best move and devises a strategy to play the least-damaging moves which maximises the chances of winning.

The function `catchUpNodeTree` handles the acquisition of the current state of the game - seeing as the AI doesn't know what the moves the opponent has made, this function ensures that the node tree is up to date. If there's no children in the current tree, possibly due to the generation mechanism stopping at a specified depth, it regenerates the tree and thinks another few moves away.

I've just finished things off tuning things, making sure that the values of each node is correct and just testing different values and grid sizes. In `Types.hs`, I've provided an implementation of the `Show` instance so that I can print out a minmax node and see its stats.

I underestimated how much processing power it takes to evaluate the node tree. A 3x3 grid is the best experience, as even thinking 2 moves ahead (a depth of 2) on a 4x4 requires more computation than calculating the entirety of a 3x3 grid. This is very unfortunate, as a 3x3 grid presents no challenge when you can simply take the middle square and win. In Hex, whoever goes first has an advantage, and this is even more evident on a 3x3 grid. Still, at least the provided executable easily demonstrates the minmax algorithm.
