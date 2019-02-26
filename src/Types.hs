module Types
( GameState (..)
, Player (..)
, Tile (..)
, Board (..)
, boardSize
, getColour
) where

-- List that allows for better insertion
import Data.Sequence hiding (Empty)

-- For colours
import System.Console.ANSI

-- A GameState which can be represented
data GameState = 
  GameState
  { board :: Board
  , turn :: Player
  }

-- Size of the game (max 10, 10)
boardSize :: (Int, Int)
boardSize = (10, 10)

-- Colours of players
getColour P1 = Cyan
getColour P2 = Red

-- 2 Players in the game
data Player = P1 | P2
  deriving (Eq)

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

-- A Tile is just a representation of a player's moves
data Tile = Empty | Stone Player
  deriving (Eq)

-- How a stone is drawn on the board
instance Show Tile where
  show _ = "o"

-- A Board is made up of tiles
newtype Board = Board (Seq Tile)
