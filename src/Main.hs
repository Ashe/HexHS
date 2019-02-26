{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.List (find)
import Data.Sequence (replicate, adjust, index)
import Data.Foldable (toList)
import Data.Char (toUpper, ord, chr)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import Control.Monad (forM_)
import Control.Concurrent.MVar
import System.Random (randomRIO)
import System.Console.ANSI
import System.IO

-- Import our types
import Types

-----------------------
-- GAME ARCHITECTURE --
-----------------------

-- Entry to our program
main :: IO ()
main = do
  ng <- newGame
  gameLoop ng
  setSGR [SetColor Foreground Vivid White]

-- Create a new game state
newGame :: IO GameState
newGame = do
  let (w, h) = boardSize
      tree = createNodeTree (w * h + 1) newBoard
  putStrLn $ "Generated tree with " ++ show (length $ children $ tree) ++ " children."
  ai <- newMVar tree
  pure $ GameState newBoard P1 Manual (AI ai)
  where newBoard = Board $ replicate size Empty
        size = let (w, h) = boardSize in w * h

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs = do
  drawBoard (board gs)
  maybeNewGS <- takeTurn gs
  case maybeNewGS of
    Just newGS -> do
      if checkWin newGS (turn gs)
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
checkWin :: GameState -> Player -> Bool
checkWin gs prevPlayer = foldl (||) False $ map check startSearch
  where board' = let (Board b) = board gs in toList b
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
  putStr " - Please make your move eg. A0: "
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

-- Create a tree of possible minmax
createNodeTree :: Int -> Board -> MinMaxNode
createNodeTree depth b@(Board board) = 
  MinMaxNode
  { current = b
  , player = P1
  , lastPlay = Nothing
  , depth = 0
  , children = leaves
  , value = 0--value . (decideMinMax P1) $ leaves
  }
  where leaves = map (constructNode depth 0 P1 b) choices
        (w, h) = boardSize
        grid = zip [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] (toList board)
        choices = map fst $ filter (((==) Empty) . snd) grid

-- Recursively create nodes and their children
constructNode :: Int -> Int -> Player -> Board -> (Int, Int) -> MinMaxNode
constructNode depth pastDepth p (Board board) (x, y) =
  MinMaxNode
  { current = newBoard
  , player = nextTurn
  , lastPlay = Just (x, y)
  , depth = newDepth
  , children = leaves
  , value = 0--value . (decideMinMax nextTurn) $ leaves
  }
  where i = y * (fst boardSize) + x
        newDepth = pastDepth + 1
        newBoard = Board $ adjust (const (Stone p)) i board
        nextTurn = if p == P1 then P2 else P1
        (w, h) = boardSize
        grid = zip [(x, y) | y <- [0..h - 1], x <- [0..w - 1]] (toList board)
        choices = map fst $ filter (((==) Empty) . snd) grid
        leaves = map (constructNode depth pastDepth nextTurn newBoard) choices
        valMult = if p == P1 then 1 else -1
        calcValue
          | length leaves > 0 = (depth + 1 - newDepth) * valMult
          | otherwise = value . (decideMinMax nextTurn) $ leaves

-- Sync up this node tree properly
catchUpNodeTree :: Board -> MinMaxNode -> Maybe MinMaxNode
catchUpNodeTree board tree 
  | current tree == board = Just tree
  | otherwise = find (\n -> current n == board) (children tree)

-- Evaluate choices and choose a random one
evaluateChoices :: MVar MinMaxNode -> GameState -> IO (Either (Int, Int) String)
evaluateChoices mvar gs = do

  -- Evaluate node tree
  nodeTree <- takeMVar mvar
  let maybeTree = catchUpNodeTree (board gs) nodeTree
  case maybeTree of

    -- Synced up correctly, resume evaluation
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

