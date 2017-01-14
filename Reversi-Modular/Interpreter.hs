module Interpreter where

import Struct
import Board
import Data.List (find)
import Text.Read (readEither)
import Commands (customCommands, maybeCommand)

eitherCustomCommand :: String -> Either Error Command
eitherCustomCommand = maybe (Left "Not valid command!") Right . maybeCommand

-- moveBoardCommand receive an indice and return a function
-- which "plays the game", but we can pretend it is a command.
moveBoardEither :: GameState -> Indice -> Either Error Command
moveBoardEither st@(GameState board (p, _)) idx =
    if canMove p board idx then undefined
                           else Left "Not a valid move."

readIdx :: String -> Either Error Indice
readIdx str
    | (x:y:"") <- str             = either (\_ -> errorE) Right $ readChar2 x y
    | ('(':x:',':y:')':"") <- str = either (\_ -> errorE) Right $ readChar2 x y
    | otherwise                   = errorE where
        readChar2 x y = (,) <$> readEither [x] <*> readEither [y]
        errorE        = Left "Not valid position!"

readEither' :: GameState -> String -> Either Error Command
readEither' _ ""   = Left "No input; try --help"
readEither' g str@(x:xs)
    | x == '-'     = eitherCustomCommand xs
    | otherwise    = either Left (moveBoardEither g) $ readIdx $ removeSpace str where
        removeSpace       = filter (/= ' ')
        detectBound idx   = if onBoard idx then Right idx else Left "Position out of range!"
