import Board
import Struct
import AI
import Text.Read (readEither)
import Data.Char (toUpper)
import Debug.Trace

-- "Pure" helper functions

isStrValidRole :: String -> Maybe Color
isStrValidRole "O" = Just White
isStrValidRole "X" = Just Black
isStrValidRole _   = Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe str
    | Left _ <- re    = Nothing
    | Right val <- re = Just val
    where
        re = readEither str

-- Side-effect (IO) functions
-- All effectful functions will be gathered here
-- XXX: actually Haskellish coding requires as much IO put in main as possible
initRole :: IO Color
initRole = do
    maybeColor <- isStrValidRole <$> getLine
    maybe (putStrLn "Not valid input!" >> initRole) onValidInput maybeColor where
        onValidInput color =
            putStrLn "Good luck!Press Anykey to start." >>
            getChar >> return color

-- XXX: Better implemented with "ExceptT" monad transformer, which will
-- turn a n-stage validity check (and early-return) into single line
humanDecision :: Color -> Board -> IO Board
humanDecision hColor board = do
    putStrLn prompt
    getLine >>= (maybe reinputFormat reactNoMove . readMaybe) -- "maybe" is nested
    where
        prompt        = "Please enter your move as (row, column), for example, (3, 4):"
        reactNoMove   = (maybe reinputNoMove return) . moveAt hColor board
        reinputFormat = putStrLn "Input format incorrect." >> humanDecision hColor board
        reinputNoMove = putStrLn "Not a valid move, dude!" >> humanDecision hColor board

aiDecision :: Color -> Board -> IO Board
aiDecision aic board = do
    putStrLn $ "AI moved at " ++ show bestMove ++ "."
    return newBoard where
    (newBoard, bestMove) = ai aic board

-- Umm this is a very neat function~
-- Return value "bool" means retry or not
loop :: Color -> Color -> Board -> IO Bool
loop humanc presentc board
    | gameEnded           = finishGame humanc board
    | presentc == humanc  = printBoard >>
                            if humanNoMove
                            then putStrLn "Can't move! Please wait for AI." >> (nextLoop board)
                            else humanDecision humanc board >>= nextLoop
    | otherwise           = printBoard >>
                            if aiNoMove
                            then putStrLn "AI can't move! Please continue." >> (nextLoop board)
                            else aiDecision (oppose humanc) board >>= nextLoop where
    printBoard  = putStr $ showBoardFor presentc board
    nextLoop    = loop humanc $ oppose presentc
    gameEnded   = humanNoMove && aiNoMove
    humanNoMove = noMove humanc board
    aiNoMove    = noMove (oppose humanc) board

finishGame :: Color -> Board -> IO Bool
finishGame humanc board = do
    putStrLn $ "You scored: " ++ show human
    putStrLn $ "AI scored: " ++ show ai
    putStrLn finalResult
    promptRetry where
        finalResult = case human `compare` ai of
            LT -> "You lose ..."
            GT -> "You win !"
            EQ -> "Tie."
        (human, ai) = if (humanc == White) then (w, b) else (b, w)
        (w, b)      = countChess board

promptRetry :: IO Bool
promptRetry = do
    putStrLn "Do you want to try again? (Y/N)"
    input <- getLine
    case (map toUpper input) of
        "Y"       -> return True
        "N"       -> return False
        otherwise -> putStrLn "Invalid input; proceed as N." >> return False

oneGame :: IO Bool
oneGame = do
    putStr welcomeStr
    humanc <- initRole
    loop humanc White initBoard where
        welcomeStr = "\nREVERSI\
                    \\nWelcome to the world of Haskell game!\
                    \\n\nWould you like to be offensive (O) or defensive (X)? \n"

main :: IO ()
main = do
    retry <- oneGame
    if retry then main else putStrLn "Quitting ..." where
