module Struct
(
    Color (..),
    State (..),
    WMove,
    BMove,
    Indice,
    oppose,
    showStateFor,
    nullState,
    isVacant,
    colorToMove
) where

-- Means both chess pieces and sides.
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

showStateFor :: Color -> State -> String
showStateFor _ (Taken White) = "O"
showStateFor _ (Taken Black) = "X"
showStateFor White (Vacant wm bm) = if (not $ null wm) then "*" else " "
showStateFor Black (Vacant wm bm) = if (not $ null bm) then "*" else " "

nullState :: State
nullState = Vacant [] []

isVacant :: State -> Bool
isVacant (Vacant _ _) = True
isVacant _ = False

colorToMove :: Color -> State -> [Indice]
colorToMove _ (Taken _) = []
colorToMove White (Vacant wm _) = wm
colorToMove Black (Vacant _ bm) = bm

-- Flag for game state
-- Human should move | AI should move | Neither can move
data GameState = HumanMove | AIMove | Ended
