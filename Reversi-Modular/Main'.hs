import Board
import AI
import Text.Read (readEither)
import Data.Char (toUpper)

-- Error is just String, in Interpreter.hs
interactive :: String -> (a -> String) -> (String -> Either Error a) -> IO String -> IO a
interactive prompt correctStrFun reader source = do
    putNonNull prompt
    input <- source
    let parsed = reader input
    case parsed of
        Left str  -> putStrLn str >> self
        Right val -> putNonNull (correctStrFun val) >> return val
    where
        self           = interactive prompt correctStrFun reader source
        putNonNull str = if null str then return () else putStrLn str

chooseRole :: IO Color
chooseRole = interactive prompt printColorName isStrValidRole getLine where
    prompt               = "\nWould you like to be offensive (O) or defensive (X)?"
    printColorName White = "You have chosen White"
    printColorName Black = "You have chosen Black"
    isStrValidRole "O" = Right White
    isStrValidRole "X" = Right Black
    isStrValidRole _   = Left "Not valid input!"

chooseMode :: IO Int
chooseMode = interactive prompt (\_ -> "OK, get that.") readEither getLine where
    prompt = "\n\n\nFirst please choose game mode: PVE (1) or PVP (2)"

initGame :: IO (Player, Player)
initGame = do
    putStrLn $ welcomeStr
    gameMode <- chooseMode
    result <- players gameMode
    putStrLn beforeGameHint
    getChar -- Press "any key"
    return result
    where
        welcomeStr       = "\nREVERSI\
                            \\n\nWelcome to the world of Haskell game!"
        noChooseHint     = "\n\nIn PVP mode you need to decide who goes first; \
                            \the first player becomes the white (O) one."
        beforeGameHint   = "\nGood luck! Press Anykey to start."
        pvePlayers White = (Human White, AI Black)
        pvePlayers Black = (AI White, Human Black)
        pvpPlayers       = (Human White, Human Black)
        players mode     = if mode == 1 then chooseRole >>= (return . pvePlayers)
                                        else putStrLn noChooseHint >> (return pvpPlayers)

humanFeedback :: Player -> Board -> IO (Board, Indice)
humanFeedback me board = interactive hint (\_ -> "") humanParse getLine where
    hint       = "\nInput example: 34 or (3,4); row first.\n\
                  \For other commands, enter --help.\n"
    readIdx str
        | (x:y:"") <- str             = either (\_ -> errorE) Right $ readChar2 x y
        | ('(':x:',':y:')':"") <- str = either (\_ -> errorE) Right $ readChar2 x y
        | otherwise                   = errorE where
            readChar2 x y = (,) <$> readEither [x] <*> readEither [y]
            errorE        = Left "Not valid position!"
    humanParse =
        either Left maybeNoMove . readIdx where
            error2 = Left "Not a valid move, dude!"
            maybeNoMove idx = maybe error2 (\board -> Right (board, idx)) $
                              moveAt me board idx

-- We write this function as so for 3 reasons:
-- 1. It can make use of interactive to output AI hint
-- 2. We need to make aiFeedback have same prototype as humanFeedback
-- 3. We can feed in random number from interactive (presently return "")
aiFeedback :: Player -> Board -> IO (Board, Indice)
aiFeedback me board = interactive "" react aiPseudoParse (return "") where
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

loop :: Game -> IO ()
loop (Game _ _ [])   = return ()
loop (Game b p c:cs) = St.runState c game' where
    game' = Game b p cs

main :: IO ()
main = loop startState where
    startState = { getBoard   = initBoard,
                   getPlayers = nullPlayers,
                   getTasks   = [initGame] }
