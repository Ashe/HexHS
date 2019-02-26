{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.Sequence (replicate, adjust, index)
import Data.Foldable (toList)
import Data.Char (toUpper, ord, chr)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import Control.Monad (forM_)
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
  gameLoop newGame
  setSGR [SetColor Foreground Vivid White]

-- Create a new game state
newGame :: GameState
newGame = GameState newBoard P1 Manual Random
  where newBoard = Board $ replicate size Empty
        size = let (w, h) = boardSize in w * h

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs = do
  drawBoard (board gs)
  newGS <- takeTurn gs
  if checkWin newGS (turn gs)
    then do
      drawBoard (board newGS)
      setSGR [SetColor Foreground Vivid (getColour (turn gs))]
      putStr $ show (turn gs)
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn " has won the game!"
    else 
      gameLoop newGS

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
takeTurn :: GameState -> IO GameState
takeTurn gs = do
  let controls = if turn gs == P1 then p1Controls gs; else p2Controls gs;
  coords <- handleControlType controls gs
  case tryMakeMove gs coords of
    Just newGS -> pure newGS
    _ -> do
      putStrLn "Error: Invalid move. Please re-evaluate strategy and try again."
      takeTurn gs

-- Play the game differently per control type
-- NOTE: This function is all about choosing where to place stones
-- and does not have any method of tainting the GameState,
-- meaning that all players abide by the same rules and conditions
handleControlType :: ControlType -> GameState -> IO (Int, Int)
handleControlType Manual = processInput
handleControlType Random = randomChoice

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
processInput :: GameState -> IO (Int, Int)
processInput gs = do
  setSGR [SetColor Foreground Vivid (getColour (turn gs))]
  putStr $ show (turn gs)
  setSGR [SetColor Foreground Vivid White]
  putStr " - Please make your move eg. A0: "
  input <- getLine
  hFlush stdout
  case convertStrToCoords input of
    Left coords -> pure coords
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

---------------------
-- RANDOM CONTROLS --
---------------------

-- Evaluate choices and choose a random one
randomChoice :: GameState -> IO (Int, Int)
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
  hFlush stdout
  pure choice
