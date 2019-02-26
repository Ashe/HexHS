{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.Sequence (replicate, adjust, index)
import Data.Char (toUpper, ord)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import System.IO

-- Import our types
import Types

-- Entry to our program
main :: IO ()
main = gameLoop newGame

-- Size of the game
boardSize :: (Int, Int)
boardSize = (11, 11)

-- Create a new game state
newGame :: GameState
newGame = GameState newBoard P1
  where newBoard = Board $ replicate size Empty
        size = let (w, h) = boardSize in w * h

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs = do
  newGS <- takePlayerTurn gs
  gameLoop newGS

-- How a player will make moves
takePlayerTurn :: GameState -> IO GameState
takePlayerTurn gs = do
  coords <- processInput gs
  case tryMakeMove gs coords of
    Just newGS -> pure newGS
    _ -> do
      putStrLn "Error: Invalid move. Please re-evaluate strategy and try again."
      takePlayerTurn gs

-- Take input of the current player and coords or error
processInput :: GameState -> IO (Int, Int)
processInput gs@(GameState board turn) = do
  putStr $ show turn ++ " - Please make your move eg. A0: "
  input <- getLine
  hFlush stdout
  case convertStrToCoords input of
    Left coords -> pure coords
    Right msg -> do
      putStrLn msg
      processInput gs

-- Converts A0 into 00
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
        newT P1 = P2
        newT P2 = P1
        newGS = 
          GameState 
            { board = Board newB
            , turn = newT t
            }

