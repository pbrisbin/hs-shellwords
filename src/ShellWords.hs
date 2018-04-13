{-# LANGUAGE OverloadedStrings #-}

module ShellWords
    ( parse
    ) where

--import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char
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
shellword = choice
    [ quoted
    , try quotedFlag -- N.B. may fail after consuming "--"
    , flagArgument
    , value
    ]

-- | A balanced, single- or double-quoted string
quoted :: Parser Text
quoted = do
    q <- oneOf ['\'', '\"']
    T.pack <$> manyTill (try (escaped q) <|> anyToken) (char q)

-- | This wierd case: @--\"foo bar\"@
quotedFlag :: Parser Text
quotedFlag = (<>)
    <$> flagPrefix
    <*> quoted

-- | A flag and (possibly quoted) argument, @--foo=\"bar\"@
flagArgument :: Parser Text
flagArgument = concat4
    <$> flagPrefix
    <*> (T.pack <$> manyTill anyToken (char '='))
    <*> pure "="
    <*> (quoted <|> value)
  where
    concat4 a b c d = a <> b <> c <> d

flagPrefix :: Parser Text
flagPrefix = string "--" <|> string "-"

-- | A bare value, here till an (unescaped) space
value :: Parser Text
value = T.pack <$> many (try (escaped ' ') <|> nonSpace)

escaped :: Char -> Parser Char
escaped c = c <$ string ("\\" <> T.singleton c)

anyToken :: Parser Char
anyToken = satisfy $ const True

nonSpace :: Parser Char
nonSpace = satisfy $ not . isSpace
