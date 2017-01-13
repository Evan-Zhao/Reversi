import Board
import Struct
import AI
import Interpreter
import Text.Read (readEither)
import Data.Char (toUpper)

interactive' :: String -> (a -> String) -> (String -> Either String a) -> IO String -> IO a
interactive' prompt correctStrFun reader source = do
    putNonNull prompt
    input <- source
    let parsed = reader input
    case parsed of
        Left str  -> putStrLn str >> self
        Right val -> putNonNull (correctStrFun val) >> return val
    where
        self           = interactive' prompt correctStrFun reader source
        putNonNull str = if null str then return () else putStrLn str

interactive :: (a -> String) -> (String -> Either String a) -> IO String -> IO a
interactive = interactive' ""

chooseRole :: IO Color
chooseRole = interactive printColorName isStrValidRole getLine where
    printColorName White = "You have chosen White"
    printColorName Black = "You have chosen Black"
    isStrValidRole "O" = Right White
    isStrValidRole "X" = Right Black
    isStrValidRole _   = Left "Not valid input!"

chooseMode :: IO Int
chooseMode = interactive (\_ -> "OK, get that.") readEither getLine

initGame :: IO (Player, Player)
initGame = do
    putStrLn $ welcomeStr ++ prompt1
    gameMode <- chooseMode
    result <- players gameMode
    putStrLn beforeGameHint
    getChar -- Press "any key"
    return result
    where
        welcomeStr       = "\nREVERSI\
                            \\n\nWelcome to the world of Haskell game!"
        prompt1          = "\n\n\nFirst please choose game mode: PVE (1) or PVP (2)"
        prompt2          = "\nWould you like to be offensive (O) or defensive (X)?"
        noChooseHint     = "\n\nIn PVP mode you need to decide who goes first; \
                            \the first player becomes the white (O) one."
        beforeGameHint   = "\nGood luck! Press Anykey to start."
        pvePlayers White = (Human White, AI Black)
        pvePlayers Black = (AI White, Human Black)
        pvpPlayers       = (Human White, Human Black)
        players mode     = if mode == 1 then putStrLn prompt2 >> chooseRole >>= (return . pvePlayers)
                                        else putStrLn noChooseHint >> (return pvpPlayers)

humanFeedback :: Player -> Board -> IO (Board, Indice)
humanFeedback me board = interactive' hint (\_ -> "") humanParse getLine where
    hint       = "\nInput example: 34 or (3,4); row first.\n\
                  \For other commands, enter --help.\n"
    humanParse =
        either (\_ -> error1) maybeNoMove . readEither where
            error1 = Left "Input format incorrect."
            error2 = Left "Not a valid move, dude!"
            maybeNoMove idx = maybe error2 (\board -> Right (board, idx)) $
                              moveAt me board idx

-- We write this function as so for 3 reasons:
-- 1. It can make use of interactive to output AI hint
-- 2. We need to make aiFeedback have same prototype as humanFeedback
-- 3. We can feed in random number from interactive (presently return "")
aiFeedback :: Player -> Board -> IO (Board, Indice)
aiFeedback me board = interactive react aiPseudoParse (return "") where
    react (_, idx)  = "AI moved at " ++ show idx ++ "."
    aiPseudoParse _ = Right $ ai (pcolor me) board

playGame :: Player -> Player -> Board -> IO Board
playGame thisP nextP board
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

finishGame :: (Player, Player) -> Board -> IO ()
finishGame (p1, p2) board = do
    putStrLn $ "Player1 scored: " ++ show p1score
    putStrLn $ "Player2 scored: " ++ show p2score
    putStrLn finalResult
    where
        finalResult = case p1score `compare` p2score of
            LT -> "Player2 win !"
            GT -> "Player1 win !"
            EQ -> "Tie."
        (p1score, p2score) = countChess board (p1, p2)

promptRetry :: IO Bool
promptRetry = do
    putStrLn "Do you want to try again? (Y/N)"
    input <- getLine
    case (map toUpper input) of
        "Y"       -> return True
        "N"       -> return False
        otherwise -> putStrLn "Invalid input; proceed as N." >> return False

main :: IO ()
main = do
    (p1, p2) <- initGame
    endBoard <- playGame p1 p2 initBoard
    finishGame (p1, p2) endBoard
    retry <- promptRetry
    if retry then main else putStrLn "Quitting ..."
