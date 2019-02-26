{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (replicate)
import Data.Sequence (replicate)
import Data.Char (toUpper, ord)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)

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
  putStrLn $ show turn ++ " - please input where you'd like to place a stone eg. A0:"
  action <- processInput
  print action
  gameLoop gs

-- Take input of the current player and coords or error
processInput :: IO (Either (Int, Int) String)
processInput = do
  input <- getLine
  pure $ convertStrToCoords input

-- Converts A0 into 00
convertStrToCoords :: String -> Either (Int, Int) String
convertStrToCoords (x : y : _) = result
  where x' = ord (toUpper x) - ord 'A'
        y' = readMaybe [y]
        result 
          | x' < 0 || x' >= snd boardSize = 
              Right $ "Error: Col " ++ [toUpper x] ++ " is invalid."
          | isNothing y' = 
              Right $ "Error: Row " ++ [y] ++ " is invalid."
          | fromJust y' < 0 || fromJust y' >= fst boardSize = 
              Right $ "Error: Row " ++ show y' ++ "is out of bounds."
          | otherwise = Left (x', fromJust y')
convertStrToCoords input = Right $ "Invalid input: " ++ input ++ "."
