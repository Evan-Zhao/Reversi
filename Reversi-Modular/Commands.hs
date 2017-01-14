module Commands where

import Struct
import Board
import Data.List (intercalate, find)

playGame :: Player -> Player -> Board -> IO Board
playGame (GameState board (p1, p2))
    | gameEnded     = return board
    | thisCantMove  = printBoard >> thisNoMoveHint >> (next board)
    | otherwise     = printBoard >> (decision thisP board >>= next) where
        printBoard      = putStr $ showBoardFor thisP board
        next board'     = playGame nextP thisP board'
        gameEnded       = thisCantMove && nextCantMove
        thisCantMove    = noMove thisP board
        nextCantMove    = noMove nextP board
        thisNoMoveHint  = putStrLn "Can't move! Please wait for your opponent."
        decision me@(Human color) board = humanFeedback me board >>= (return . fst)
        decision me@(AI color) board    = aiFeedback me board >>= (return . fst)

customCommands :: [(String, String, Command)]
customCommands =
    [ ("help", helpStr, helpC)
    , ("quit", quitStr, quitC)
    , ("show", showStr, showC)
    , ("main", mainStr, mainC)
    , ("score", scoreStr, scoreC) ]

maybeCommand :: String -> Maybe Command
maybeCommand str = (\(_,_,c) -> c) <$> find (\(s,_,_) -> s == str) customCommands

helpStr = "Display this information"
quitStr = "Quit the program immidiately"
showStr = "Print the reversi board again"
mainStr = "End present game and display welcome message"
scoreStr = "Calculate and display the score of two players"

helpC :: GameState -> IO GameState
helpC g = putStrLn (helpText 40) >> (return g)

helpText :: Int -> String
helpText width = concat $ map singleHelpText customCommands where
    lpWidth          = 4
    padL             = replicate lpWidth ' '
    seperator        = " | "
    eqWideName str   = padL ++ str ++ replicate (nameMaxLen - length str) ' '
    nameMaxLen       = foldr (\(name, _, _) acc -> max acc $ (length name + 1)) 0 customCommands
    rightColumnW     = width - (lpWidth + nameMaxLen + length seperator)
    singleHelpText (n,s,_) = eqWideName ("-" ++ n) ++ (
                             intercalate (eqWideName "") $
                             map makeupStr $ cut (words s) rightColumnW
                             )
    makeupStr str = seperator ++ str ++ "\n"


cut :: [String] -> Int -> [String]
cut word w = if totalLen word >= w then (unwords words1) : (cut words2 w)
                                   else [unwords word] where
    totalLen strs = sum $ map length strs
    (words1, words2) = spanRange word w

spanRange :: [[a]] -> Int -> ([[a]], [[a]])
spanRange [] _  = ([], [])
spanRange (x:xs) n
    | n <= 0    = ([], (x:xs))
    | otherwise = x `consFirst` (spanRange xs $ n - length x) where
        x `consFirst` (a,b) = (x:a, b)

quitC = undefined
showC = undefined
mainC = undefined
scoreC = undefined
