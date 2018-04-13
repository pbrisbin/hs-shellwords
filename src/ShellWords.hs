{-# LANGUAGE OverloadedStrings #-}

module ShellWords
    ( parse
    ) where

import Data.Bifunctor (first)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parse :: Text -> Either String [Text]
parse = first parseErrorPretty . Megaparsec.parse parser "<input>"

parser :: Parser [Text]
parser = shellword `sepBy` space1

shellword :: Parser Text
shellword = choice [quoted, shelloption, value]

-- | A balanced, single- or double-quoted string
quoted :: Parser Text
quoted = do
    q <- oneOf ['\'', '\"']
    T.pack <$> manyTill (try (escaped q) <|> anyToken) (char q)

-- | A flag, with or without an argument
shelloption :: Parser Text
shelloption = (<>)
    <$> flag
    <*> (fromMaybe "" <$> optional argument)

-- | A flag like @--foo@, or (apparently) @--\"baz bat\"@
flag :: Parser Text
flag = (<>)
    <$> (string "--" <|> string "-")
    <*> (quoted <|> (T.pack <$> many (noneOf ['=', ' '])))

-- | The argument to a flag like @=foo@, or @=\"baz bat\"@
argument :: Parser Text
argument = (<>)
    <$> (T.singleton <$> char '=')
    <*> (quoted <|> value)

-- | A plain value, here till an (unescaped) space
value :: Parser Text
value = T.pack <$> many (try (escaped ' ') <|> nonSpace)

escaped :: Char -> Parser Char
escaped c = c <$ string ("\\" <> T.singleton c)

anyToken :: Parser Char
anyToken = satisfy $ const True

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
