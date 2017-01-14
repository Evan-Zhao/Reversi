module Board
(
    module Structure
    n,
    initBoard,
    showBoardFor,
    onBoard,
    moveAt,
    noMove,
    canMove,
    countChess,
    indices
) where

import qualified Data.Map.Strict as M
import Data.List (intersperse, find, dropWhile)
import Structure

-- type Board = M.Map Indice State, in Struct.hs

-- Structure of board:
-- Size
n = 8 -- Board size is n*n
-- Topology (as directions)
dX :: [Int]
dX = [-1,-1,-1,0,0,1,1,1]
dY :: [Int]
dY = [-1,0,1,-1,1,-1,0,1]
type Direction = Int

-- Encapsulating (short functions which hides internal map structure)
indices :: [Indice]
indices = [ (i,j) | i <- [0..(n-1)],  j <- [0..(n-1)] ]

rows :: Board -> [(Board, Int)]
rows board = zip (map (flip row board) [0..(n-1)]) [0..(n-1)] where
    row n = M.filterWithKey (\(x,y) _ -> x == n)

flattenedAt :: Indice -> Direction -> [Indice]
flattenedAt (i, j) dir = takeWhile onBoard [ (i + k * (dX!!dir), j + k * (dY!!dir)) | k <- [1..] ]

-- Functionality (which a board should have but a map should not)
initBoard :: Board
initBoard = updateMove $ nonBlank `M.union` blank where -- Union prefers first
    blank = M.fromList [ ((i,j), nullState) | i <- [0..(n-1)], j <- [0..(n-1)] ]
    nonBlank = M.fromList [ ((n'-1, n'-1), Taken Black), ((n'-1, n'), Taken White),
                            ((n', n'-1), Taken White), ((n', n'), Taken Black) ]
    n' = n `div` 2

showBoardFor :: Player -> Board -> String
showBoardFor player = (firstLine ++) . (seperateWith splitter) . (map $ uncurry showRowFor) . rows where
    showRowFor row n = show n ++ (seperateWith " | " $ map (showStateFor player) $ elements row) ++ "\n"
    firstLine        = spaces 4 ++ (concat $ intersperse (spaces 3) $ map show [0..(n-1)]) ++ "\n"
    splitter         = spaces 2 ++ (concat $ outsperse "+" $ replicate n "---") ++ "\n";
    elements         = foldr (:) []
    seperateWith str = concat . outsperse str
    outsperse x xs   = x: intersperse x xs ++ [x]
    spaces n         = replicate n ' '

-- Update move state after chess is add/removed from board
-- Internal
updateMove :: Board -> Board
updateMove board = foldr updateSingleSt board indices where
    updateSingleSt idx board = M.adjustWithKey (newMoveState board) idx board

newMoveState :: Board -> Indice -> State -> State
newMoveState board idx this = if isVacant this then (Vacant wmove bmove) else this where
    wmove = flipable White board idx
    bmove = flipable Black board idx

reversiSpan :: Color -> Board -> Indice -> Direction -> [Indice]
reversiSpan color board idx dir = if isValidDir then takenIndices else [] where
    isValidDir = (not $ null indices) &&
                 (not $ null droppedState) &&
                 (head state == Taken (oppose color)) &&
                 (head droppedState == Taken color)
    state = map (board M.!) indices
    droppedState = dropWhile (== Taken (oppose color)) state
    takenIndices = takeWhile (\idx -> board M.! idx == Taken (oppose color)) indices
    indices = flattenedAt idx dir

flipable :: Color -> Board -> Indice -> [Indice]
flipable color board idx = foldr flipableDir [] [0..7] where
    flipableDir dir accum = accum ++ (reversiSpan color board idx dir)

-- Public
moveAt :: Player -> Board -> Indice -> Maybe Board
moveAt player board idx = if null flipIndices then Nothing
                            else Just $ updateMove $ newMap `M.union` board where
    flipIndices = moveOfPlayer player (board M.! idx) -- These grids must have been taken by (oppose color)
    allIndices  = idx : flipIndices -- because we also place chess at given idx
    newMap      = M.fromList $ zip allIndices $ repeat $ myChess player
    color       = pcolor player

canMove :: Player -> Board -> Indice -> Bool
canMove player board idx = null flipIndices where
    flipIndices = moveOfPlayer player (board M.! idx)

onBoard :: Indice -> Bool
onBoard (i, j) = (i >= 0 && i < n && j >= 0 && j < n)

noMove :: Player -> Board -> Bool
noMove player = and . (M.map $ gridNotValid player) where
    gridNotValid player state = null $ moveOfPlayer player state

countChess :: Board -> (Player, Player) -> (Int, Int)
countChess board (p1, p2) = (countPlayerChess p1 board, countPlayerChess p2 board) where
    countPlayerChess player = M.size . M.filter (== myChess player)
