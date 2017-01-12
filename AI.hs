module AI (ai) where

import qualified Data.Map.Strict as M
import Board
import Struct

-- Weight function
weightQuarter = [ [7,5,5,5], [5,3,1,1], [5,1,1,1], [5,1,1,1] ]
noMovePenalty = 3

weight :: Indice -> Int
weight (i,j) = weightQuarter !! (i `min` (n-1-i)) !! (j `min` (n-1-j))

evaluate :: Color -> Board -> Int
evaluate color board = occupyScore - (if noMove color board then noMovePenalty else 0) where
    occupyScore = foldl (+) 0 $ M.mapWithKey scoreSingle board
    scoreSingle idx state
        | state == Taken color          = weight idx
        | state == Taken (oppose color) = -weight idx
        | otherwise                       = 0

-- Search
maxDepth = 3

extract :: Maybe a -> a
extract (Just x) = x

retrace :: Color -> Color -> Int -> Board -> (Int, Indice)
retrace aic    _     0     board = (evaluate aic board, (-1,-1))
retrace aic presentc depth board
    | null subScores = retrace aic (oppose presentc) (depth-1) board
    | otherwise      = choiceFun $ zip subScores subIdx where
        choiceFun               = (if aic == presentc then maximum else minimum)
        subScores               = map (fst . retrace aic (oppose presentc) (depth-1)) subBoards
        subBoards               = map extract subBoardsJust
        (subBoardsJust, subIdx) = unzip $ filter (\(b, i) -> b /= Nothing) $
                                zip (map (moveAt presentc board) indices) indices

ai :: Color -> Board -> (Board, Indice)
ai aic board = (extract $ moveAt aic board bestMove, bestMove) where
    bestMove = snd $ retrace aic aic maxDepth board
