{-# LANGUAGE OverloadedStrings #-}

module ShellWords
    ( parse
    ) where

--import Control.Monad (void)
import Data.Char
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text as T
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Text Text

parse :: Text -> Either (ParseError Char Text) [Text]
parse = Megaparsec.parse parser "<input>"

parser :: Parser [Text]
parser = shellword `sepBy` space1

shellword :: Parser Text
shellword = shelloption <|> shellargument

shelloption :: Parser Text
shelloption = string "nope"

shellargument :: Parser Text
shellargument = quoted anyToken <|> unquotedValue

quoted :: Parser Char -> Parser Text
quoted p = do
    q <- oneOf ['\'', '\"']
    T.pack <$> manyTill (try (escaped q) <|> p) (char q)

unquotedValue :: Parser Text
unquotedValue = T.pack <$> many (try (escaped ' ') <|> nonSpace)

escaped :: Char -> Parser Char
escaped c = c <$ string ("\\" <> T.singleton c)

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace

anyToken :: Parser Char
anyToken = satisfy $ const True
