module Types
( GameState (..)
, Player (..)
, Tile (..)
, Board
) where

-- List that allows for better insertion
import Data.Sequence

-- A GameState which can be represented
data GameState = 
  GameState
  { board :: Board
  , turn :: Player
  }

-- 2 Players in the game
data Player = P1 | P2
  deriving (Eq, Show)

-- A Tile is just a representation of a player's moves
data Tile = Empty | Stone Player
  deriving (Eq)

-- A Board is made up of tiles
type Board = Seq Tile
