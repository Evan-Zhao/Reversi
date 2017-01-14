module Structure where

import qualified Data.Map.Strict as M
import qualified Control.Monad.State as St

data Color = White | Black deriving (Eq, Show)
oppose :: Color -> Color
oppose White = Black
oppose Black = White

-- Indices on board (view Board.hs for usage)
type Indice = (Int, Int)

-- The indices of grids that will be flipped if one (black/white) take this grid
type WMove = [Indice]
type BMove = [Indice]

-- Possible states on a grid of chessboard
-- Every grid is either taken by White/Black, or
-- vacant with some (or no) grids potentially be flipped, where
-- in later case this grid cannot be taken.
data State = Taken Color | Vacant WMove BMove deriving (Eq, Show)

showStateFor :: Player -> State -> String
showStateFor _ (Taken White) = "O"
showStateFor _ (Taken Black) = "X"
showStateFor player (Vacant wm bm)
    | pcolor player == White = if (not $ null wm) then "*" else " "
    | otherwise              = if (not $ null bm) then "*" else " "

nullState :: State
nullState = Vacant [] []

isVacant :: State -> Bool
isVacant (Vacant _ _) = True
isVacant _ = False

-- Game board: board.hs
type Board = M.Map Indice State

-- Player identity
data Player = Human {pcolor :: Color} | AI {pcolor :: Color}

moveOfPlayer :: Player -> State -> [Indice]
moveOfPlayer _ (Taken _) = []
moveOfPlayer player (Vacant wm bm)
    | pcolor player == White = wm
    | otherwise              = bm

myChess :: Player -> State
myChess player
    | pcolor player == White = Taken White
    | otherwise              = Taken Black

oppoChess :: Player -> State
oppoChess player
    | pcolor player == White = Taken Black
    | otherwise              = Taken White

-- First player in tuple is meant to be either the first (initially)
-- or the present one (in the run) to move.
type Players = (Player, Player)

-- This is a randomly generated player tuple since we don't really need
-- Maybe Players type
nullPlayers :: Players
nullPlayers = (Human White, Human Black)

-- This is the whole state of game.
data Game = Game { getBoard :: Board, getPlayers :: Players, getTasks :: [Command] }

type Message = String

-- We'll have a bunch of functions which takes Game as global state, meanwhile may return a Message.
type Command = St.State Game Message
