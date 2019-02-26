{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.List (find)
import Data.Sequence (replicate, adjust, index)
import Data.Foldable (toList)
import Data.Char (toUpper, ord, chr)
import Text.Read (readMaybe)
import Data.Maybe (isJust, isNothing, fromJust)
import Control.Monad (forM_)
import Control.Concurrent.MVar
import System.Random (randomRIO)
import System.Console.ANSI
import System.IO

-- Import our types
import Types

--------------------------
-- MENUS AND INTERFACES --
--------------------------

-- Entry to our program
main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn "\nHEX:"
  setSGR [SetColor Foreground Vivid White]
  putStrLn "\nMain Menu:"
  putStrLn "Press any key to play, 'R' for rules or 'Q' to quit."
  str <- getLine
  let choice = if length str > 0 then toUpper (head str) else ' '
  case choice of
    'Q' -> putStrLn "Goodbye!"
    'R' -> do
      putStrLn "By specifying empty tiles, place stones and traverse the board."
      setSGR [SetColor Foreground Vivid (getColour P1)]
      putStr "Player 1 "
      setSGR [SetColor Foreground Vivid White]
      putStrLn "wins by connecting the left and right sides together."
      setSGR [SetColor Foreground Vivid (getColour P2)]
      putStr "Player 2 "
      setSGR [SetColor Foreground Vivid White]
      putStrLn "wins by connecting the top and bottom sides together."
      main
    _ -> do
      ng <- setupGame
      gameLoop ng
      main
  setSGR [SetColor Foreground Vivid White]

-- Allow the user to set up the game
setupGame :: IO GameState
setupGame = do
  putStrLn "\nNew Game: "
  setSGR [SetColor Foreground Vivid White]
  let board = newGameBoard
      getController i = do
        putStrLn $ "Please choose how player " ++ show i ++ " will play: "
        putStrLn "A: Manual Input"
        putStrLn "B: Random Input"
        putStrLn "C: Minmax Calculated"
        str <- getLine
        if length str == 1 then
          case toUpper (head str) of
            'A' -> pure Manual
            'B' -> pure Random
            'C' -> do
              let complexity = fst boardSize * snd boardSize
                  tree = constructNode complexity (-1) P1 board Nothing
              ai <- newMVar tree
              pure $ AI ai
            _ -> do
              putStrLn "Error - Try again."
              getController i
        else do
          putStrLn "Error - Try again."
          getController i
  c1 <- getController 1
  c2 <- getController 2
  pure $ GameState board P1 c1 c2

-- Create a new game state
newGameBoard :: Board
newGameBoard = Board $ replicate size Empty
  where size = let (w, h) = boardSize in w * h

-- Draw the state of the board on screen
drawBoard :: Board -> IO ()
drawBoard (Board ls) = br >> forM_ [0..h + 1] drawRow >> br
  where br = putStrLn ""
        (w, h) = boardSize
        offset r = putStr $ take r $ repeat ' '
        drawRow r = offset r >> (forM_ [0..w + 1] (drawEl r)) >> br
        drawEl y x
          | (y <= 0 && x <= 0) || (x > w && y > h) = putStr "  "
          | (y > h && x <= 0) || (x > w && y <= 0) = putStr "  "
          | (y == 0 || y > h) && x > 0 =
            p2Col >> putStr [' ', chr (x - 1 + (ord 'A'))]
          | (x == 0 || x > w) && y > 0 =
            p1Col >> putStr (" " ++ show (y - 1))
          | otherwise = do
              let el = getEl (x - 1) (y - 1)
                  col = case el of
                          Empty -> White
                          Stone pl -> getColour pl
              setSGR [SetColor Foreground Vivid col]
              putStr $ " " ++ show el
        getEl x y = index ls (fst boardSize * y + x)
        p1Col = setSGR [SetColor Foreground Vivid (getColour P1)]
        p2Col = setSGR [SetColor Foreground Vivid (getColour P2)]

-----------------------
-- GAME ARCHITECTURE --
-----------------------

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs = do
  drawBoard (board gs)
  maybeNewGS <- takeTurn gs
  case maybeNewGS of
    Just newGS -> do
      if checkWin (board newGS) (turn gs)
        then do
          drawBoard (board newGS)
          setSGR [SetColor Foreground Vivid (getColour (turn gs))]
          putStr $ show (turn gs)
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn " has won the game!"
        else
          gameLoop newGS
    _ ->
      putStrLn "An error has occured. Closing."

-- How a player will make moves
takeTurn :: GameState -> IO (Maybe GameState)
takeTurn gs = do
  let controls = if turn gs == P1 then p1Controls gs; else p2Controls gs;
  tryCoords <- handleControlType controls gs
  case tryCoords of
    Left coords ->
      case tryMakeMove gs coords of
        Just newGS -> pure $ Just newGS
        _ -> do
          putStrLn "Error: Invalid move. Please re-evaluate strategy and try again."
          takeTurn gs
    Right msg -> do
      setSGR [SetColor Foreground Vivid White]
      putStrLn msg
      pure Nothing

-- Play the game differently per control type
-- NOTE: This function is all about choosing where to place stones
-- and does not have any method of tainting the GameState,
-- meaning that all players abide by the same rules and conditions
handleControlType :: ControlType -> GameState -> IO (Either (Int, Int) String)
handleControlType Manual = processInput
handleControlType Random = randomChoice
handleControlType (AI nodes) = evaluateChoices nodes

-- Check if the tile at coords is unoccupied
tryMakeMove :: GameState -> (Int, Int) -> Maybe GameState
tryMakeMove gs (x, y) =
  case index b i of
    Empty -> Just newGS
    _ -> Nothing
  where t = turn gs
        Board b = board gs
        i = y * (fst boardSize) + x
        newB = adjust (const (Stone t)) i b
        newGS =
          gs
            { board = Board newB
            , turn = if t == P1 then P2 else P1
            }

-- Check if the previous player won
checkWin :: Board -> Player -> Bool
checkWin (Board b) prevPlayer = foldl (||) False $ map check startSearch
  where board' = toList b
        (w, h) = boardSize
        grid = zip [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] board'
        check = checkWinChain grid prevPlayer []
        startSearch = if prevPlayer == P1
                        then [(0, n) | n <- [0..h - 1]]
                        else [(n, 0) | n <- [0..h - 1]]

-- Searches all neighbours recursively to find victory
checkWinChain :: [((Int, Int), Tile)] -> Player -> [(Int, Int)] -> (Int, Int) -> Bool
checkWinChain grid p history search@(x, y)
  | lookup search grid /= Just (Stone p) = False
  | goal search = True
  | otherwise = foldl (||) False $ map check searches'
  where (w, h) = boardSize
        goal (x, y) = (p == P1 && x == w - 1) || (p == P2 && y == h - 1)
        check = checkWinChain grid p (history ++ [search])
        searches = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], i /= j]
        searches' = filter (\s@(x, y) -> x >= 0
                            && x < w && y >= 0
                            && y < h
                            && notElem s history) searches

---------------------
-- MANUAL CONTROLS --
---------------------

-- Take input of the current player and coords
processInput :: GameState -> IO (Either (Int, Int) String)
processInput gs = do
  setSGR [SetColor Foreground Vivid (getColour (turn gs))]
  putStr $ show (turn gs)
  setSGR [SetColor Foreground Vivid White]
  putStrLn " - Please make your move eg. A0: "
  input <- getLine
  hFlush stdout
  case convertStrToCoords input of
    Left coords -> pure $ Left coords
    Right msg -> do
      putStrLn msg
      processInput gs

-- Finds coords from a string eg A0 into (0, 0) or returns error
convertStrToCoords :: String -> Either (Int, Int) String
convertStrToCoords (x : y : _)
  | x' < 0 || x' >= snd boardSize =
      Right $ "Error: Col " ++ [toUpper x] ++ " is invalid."
  | isNothing y' =
      Right $ "Error: Row " ++ [y] ++ " is invalid."
  | fromJust y' < 0 || fromJust y' >= fst boardSize =
      Right $ "Error: Row " ++ show y' ++ "is out of bounds."
  | otherwise = Left (x', fromJust y')
  where x' = ord (toUpper x) - ord 'A'
        y' = readMaybe [y]
convertStrToCoords input = Right $ "Error: Invalid input \"" ++ input ++ "\"."

-------------------
-- RANDOM CHOICE --
-------------------

-- Evaluate choices and choose a random one
randomChoice :: GameState -> IO (Either (Int, Int) String)
randomChoice gs = do
  let board' = let (Board b) = board gs in toList b
      (w, h) = boardSize
      grid = zip [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] board'
      choices = filter (((==) Empty) . snd) grid
  setSGR [SetColor Foreground Vivid (getColour (turn gs))]
  putStr $ show (turn gs)
  setSGR [SetColor Foreground Vivid White]
  putStr " - Randomly chooses tile: "
  rand <- randomRIO (0, (length choices) - 1)
  let choice@(x, y) = fst (choices !! rand)
  putStrLn $ [chr (x + ord 'A')] ++ show y
  pure $ Left choice

---------------------
-- MINMAX AI LOGIC --
---------------------

-- Recursively create nodes and their children
-- This is very bloated:
  -- current: Current state of the board
  -- player: Who's going to make their turn next
  -- lastPlay: What was the last played move by the previous person
-- The previous move impacts the current board, marked with the opposing player
constructNode :: Int -> Int -> Player -> Board -> Maybe (Int, Int) -> MinMaxNode
constructNode depth pastDepth p (Board board) maybeCoords =
  MinMaxNode
  { current = Board newBoard
  , player = p
  , lastPlay = maybeCoords
  , depth = newDepth
  , children = leaves
  , value = calcValue
  }
  where i = let Just (x, y) = maybeCoords in y * (fst boardSize) + x
        newDepth = pastDepth + 1
        newBoard
          | isJust maybeCoords = adjust (const (Stone nextTurn)) i board
          | otherwise = board
        nextTurn = if p == P1 then P2 else P1
        (w, h) = boardSize
        grid = zip [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] (toList newBoard)
        choices = map fst $ filter (((==) Empty) . snd) grid
        leaves
          | newDepth >= depth = []
          | otherwise = map (constructNode depth newDepth nextTurn (Board newBoard)) (map Just choices)
        valMult = if p == P1 then -1 else 1
        calcValue
          | checkWin (Board newBoard) p = resolveValue
          | newDepth >= depth = resolveValue
          | length leaves > 0 = value . (decideMinMax p) $ leaves
          | otherwise = resolveValue
        resolveValue = (depth + 1 - newDepth) * valMult

-- Sync up this node tree properly
catchUpNodeTree :: Board -> MinMaxNode -> Maybe MinMaxNode
catchUpNodeTree board tree
  | current tree == board = Just tree
  | length (children tree) == 0 = Just $
      constructNode (depth tree) (-1) (player tree) (current tree) (lastPlay tree)
  | otherwise = find (\n -> current n == board) (children tree)

-- Evaluate choices and choose a random one
evaluateChoices :: MVar MinMaxNode -> GameState -> IO (Either (Int, Int) String)
evaluateChoices mvar gs = do

  -- Evaluate node tree
  nodeTree <- takeMVar mvar
  let maybeTree = catchUpNodeTree (board gs) nodeTree

  -- Print setup
  setSGR [SetColor Foreground Vivid (getColour (turn gs))]
  putStr $ show (turn gs)
  setSGR [SetColor Foreground Vivid White]
  putStrLn " - Thinking.." >> print nodeTree
  setSGR [SetColor Foreground Vivid Magenta]
  forM_ (children nodeTree) (\c -> putStrLn $ "Child" ++ show c)

  -- Try to sync up
  case maybeTree of

    -- Synced up correctly, resume evaluation
    -- If this new tree has no more children, generate more of the tree
    -- The values of this tree are recalculated, so messing with the depth doesn't matter
    Just tree -> do
      let nextTree = decideMinMax (player tree) (children tree)
          maybeCoords = lastPlay nextTree
      case maybeCoords of

        -- Report findings
        Just (x, y) -> do
          putMVar mvar nextTree
          setSGR [SetColor Foreground Vivid (getColour (turn gs))]
          putStr $ show (turn gs)
          setSGR [SetColor Foreground Vivid White]
          putStr " - chooses tile: "
          putStrLn $ [chr (x + ord 'A')] ++ show y
          pure $ Left (x, y)

        -- Couldn't get coords, failed
        _ -> do
          putMVar mvar nodeTree
          pure $ Right "Critical Error: Couldn't find what move to play."

    -- Failed, somethings happening
    _ -> do
      putMVar mvar nodeTree
      pure $ Right "Critical Error: Couldn't sync node tree."
