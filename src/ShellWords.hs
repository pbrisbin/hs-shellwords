{-# LANGUAGE OverloadedStrings #-}

module ShellWords
  ( parse
  , parseText

    -- * Low-level parser
  , Parser
  , runParser
  , parser
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.Char
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Compat hiding (parse, runParser)

type Parser = Parsec Void String

parse :: String -> Either String [String]
parse = runParser parser

runParser :: Parser a -> String -> Either String a
runParser p = first errorBundlePretty . Megaparsec.parse p "<input>"

-- | Parse and return @'Text'@ values
parseText :: Text -> Either String [Text]
parseText = fmap (map pack) . parse . unpack

parser :: Parser [String]
parser = fmap (dropWhileEnd null) $ space *> shellword `sepEndBy1` space1

shellword :: Parser String
shellword =
  choice
    [ quoted <?> "quoted string"
    , shelloption <?> "shell option"
    , value <?> "bare value"
    ]
    <?> "shell word"

-- | A balanced, single- or double-quoted string
quoted :: Parser String
quoted = do
  q <- oneOf ['\'', '\"']
  manyTill (escaped q <|> anyToken) $ char q

-- | A flag, with or without an argument
shelloption :: Parser String
shelloption = (<>) <$> flag <*> (fromMaybe "" <$> optional argument)

-- brittany-disable-next-binding

-- | A flag like @--foo@, or (apparently) @--\"baz bat\"@
flag :: Parser String
flag =
  (<>)
    <$> (string "--" <|> string "-")
    <*> (quoted <|> value)

-- | The argument to a flag like @=foo@, or @=\"baz bat\"@
argument :: Parser String
argument = (:) <$> char '=' <*> (quoted <|> value)

-- | A plain value, here till an (unescaped) space
value :: Parser String
value = many nonSpaceNonReserved

escaped :: Char -> Parser Char
escaped c = c <$ (escapedSatisfy (== c) <?> "escaped" <> show c)

escapedSpace :: Parser Char
escapedSpace = escapedSatisfy isSpace <?> "escaped white space"

escapedAnyOf :: [Char] -> Parser Char
escapedAnyOf cs = escapedSatisfy (`elem` cs) <?> "escaped one of " <> cs

escapedSatisfy :: (Char -> Bool) -> Parser Char
escapedSatisfy p = try $ string "\\" *> satisfy p

anyToken :: Parser Char
anyToken = satisfy $ const True

nonSpaceNonReserved :: Parser Char
nonSpaceNonReserved =
  escapedSpace
    <|> escapedAnyOf reserved
    <|> satisfy (\c -> not $ isSpace c || isReserved c)
    <?> "non white space / non reserved character"

isReserved :: Char -> Bool
isReserved = (`elem` reserved)

reserved :: [Char]
reserved = "();="
