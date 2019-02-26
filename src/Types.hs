module Types
( GameState (..)
, Player (..)
, Tile (..)
, Board (..)
) where

-- List that allows for better insertion
import Data.Sequence hiding (Empty)

-- A GameState which can be represented
data GameState = 
  GameState
  { board :: Board
  , turn :: Player
  }

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
  show Empty = "."
  show (Stone P1) = "1"
  show (Stone P2) = "2"

-- A Board is made up of tiles
newtype Board = Board (Seq Tile)
