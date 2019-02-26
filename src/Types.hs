module Types
( GameState (..)
, Player (..)
, ControlType (..)
, Tile (..)
, Board (..)
, MinMaxNode (..)
, boardSize
, getColour
, decideMinMax
) where

-- List that allows for better insertion
import Data.Sequence hiding (Empty)

-- Allows for a mutable value
import Control.Concurrent.MVar (MVar)

-- For colours
import System.Console.ANSI

--------------------
-- GAME CONSTANTS --
--------------------

-- Size of the game (max 10, 10)
boardSize :: (Int, Int)
boardSize = (5, 5)

-- Colours of players
getColour P1 = Cyan
getColour P2 = Red

-- How to process the values of node trees
decideMinMax :: Ord a => Player -> ([a] -> a)
decideMinMax P1 = minimum
decideMinMax P2 = maximum

----------------
-- GAME TYPES --
----------------

-- A GameState which can be represented
data GameState = 
  GameState
  { board       :: Board
  , turn        :: Player
  , p1Controls  :: ControlType
  , p2Controls  :: ControlType
  }

-- 2 Players in the game
data Player = P1 | P2
  deriving (Eq)

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

-- Ways of controlling the game
data ControlType = Manual | Random | AI (MVar MinMaxNode)

-- A Tile is just a representation of a player's moves
data Tile = Empty | Stone Player
  deriving (Eq)

-- How a stone is drawn on the board
instance Show Tile where
  show _ = "o"

-- A Board is made up of tiles
newtype Board = Board (Seq Tile)
  deriving (Eq, Show)

--------------
-- AI TYPES --
--------------

-- Node of a minmax search tree
data MinMaxNode =
  MinMaxNode
  { current   :: Board
  , player    :: Player
  , children  :: [MinMaxNode]
  , lastPlay  :: Maybe (Int, Int)
  , value     :: Int
  , depth     :: Int
  }

-- Display the tree's values only
instance Show MinMaxNode where
  show a = "Node: { Value: " ++ show (value a) ++ ", Children: " ++ show (children a) ++ "}"

-- Make sure we can compare nodes
instance Eq MinMaxNode where
  (==) a b = value a == value b

-- Ensure we can rank nodes
instance Ord MinMaxNode where
  compare a b = compare (value a) (value b)
  (<) a b = value a < value b
  (<=) a b = value a <= value b
  (>) a b = value a > value b
  (>=) a b = value a >= value b
