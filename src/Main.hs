{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.Sequence (replicate, adjust, index)
import Data.Char (toUpper, ord, chr)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import Control.Monad (forM_)
import System.Console.ANSI
import System.IO

-- Import our types
import Types

-- Entry to our program
main :: IO ()
main = gameLoop newGame

-- Create a new game state
newGame :: GameState
newGame = GameState newBoard P1
  where newBoard = Board $ replicate size Empty
        size = let (w, h) = boardSize in w * h

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs = do
  drawBoard (board gs)
  newGS <- takePlayerTurn gs
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
takePlayerTurn :: GameState -> IO GameState
takePlayerTurn gs = do
  coords <- processInput gs
  case tryMakeMove gs coords of
    Just newGS -> pure newGS
    _ -> do
      putStrLn "Error: Invalid move. Please re-evaluate strategy and try again."
      takePlayerTurn gs

-- Take input of the current player and coords
processInput :: GameState -> IO (Int, Int)
processInput gs@(GameState board turn) = do
  let col = case turn of P1 -> Blue; P2 -> Red;
  setSGR [SetColor Foreground Vivid White]
  putStr $ show turn ++ " - Please make your move eg. A0: "
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
          GameState 
            { board = Board newB
            , turn = if t == P1 then P2 else P1
            }

