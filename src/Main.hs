{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.Sequence (replicate)
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
  where newBoard = replicate size Empty
        size = let (w, h) = boardSize in w * h

-- Game loop of game
gameLoop :: GameState -> IO ()
gameLoop !gs@(GameState board turn) = do
  coords <- processInput turn
  print coords
  gameLoop gs

-- Take input of the current player and coords or error
processInput :: Player -> IO (Int, Int)
processInput turn = do
  putStr $ show turn ++ " - Please make your move eg. A0: "
  input <- getLine
  hFlush stdout
  case convertStrToCoords input of
    Left coords -> pure coords
    Right msg -> do
      putStrLn msg
      processInput turn

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
