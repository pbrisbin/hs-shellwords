module ShellWords
    ( parse
    ) where

parse :: String -> Either String [String]
parse = Right . words
