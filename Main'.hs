-- This is a refactoring version of main.

oneGame' :: IO Bool
oneGame' = do
    putStr welcomeStr
    humanc <- initRole
    finalBoard <- loop' humanc White initBoard
    finishGame humanc finalBoard where
        welcomeStr = "\nREVERSI\
                    \\nWelcome to the world of Haskell game!\
                    \\n\nWould you like to be offensive (O) or defensive (X)? \n"

loop' :: Color -> Color -> Board -> IO Board
loop' humanc presentc board
    | gameEnded           = return board
    | presentc == humanc  = printBoard >>
                            if humanNoMove
                            then putStrLn "Can't move! Please wait for AI." >> (nextLoop board)
                            else humanDecision humanc board >>= nextLoop
    | otherwise           = printBoard >>
                            if aiNoMove
                            then putStrLn "AI can't move! Please continue." >> (nextLoop board)
                            else aiDecision (oppose humanc) board >>= nextLoop where
    printBoard  = putStr $ showBoardFor presentc board
    nextLoop    = loop' humanc $ oppose presentc
    gameEnded   = humanNoMove && aiNoMove
    humanNoMove = noMove humanc board
    aiNoMove    = noMove (oppose humanc) board

humanDecision' :: Color -> Board -> String -> Either String Board
humanDecision' hColor board =
    either (\_ -> error1) maybeNoMove . readEither where
        error1 = Left "Input format incorrect."
        error2 = Left "Not a valid move, dude!"
        maybeNoMove = maybe error2 Right . moveAt hColor board
