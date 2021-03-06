module Struct where

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

-- Finally, player identity
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
