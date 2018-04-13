module ShellWords
    ( parse
    ) where

import Data.Text (Text)
import qualified Data.Text as T

type ParseError = Text

parse :: Text -> Either ParseError [Text]
parse = Right . T.words
