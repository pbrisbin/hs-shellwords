{-# LANGUAGE OverloadedStrings #-}

module ShellWords
    ( parse
    , parseText
    ) where

import Data.Bifunctor (first)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

parse :: String -> Either String [String]
parse = first errorBundlePretty . Megaparsec.parse parser "<input>" . strip
    where strip = let f = reverse . dropWhile isSpace in f . f

-- | Parse and return @'Text'@ values
parseText :: Text -> Either String [Text]
parseText = fmap (map T.pack) . parse . T.unpack

parser :: Parser [String]
parser = shellword `sepBy` space1

shellword :: Parser String
shellword = choice [quoted, shelloption, value]

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
        <*> (quoted <|> many (nonSpaceOr '='))

-- | The argument to a flag like @=foo@, or @=\"baz bat\"@
argument :: Parser String
argument = (:) <$> char '=' <*> (quoted <|> value)

-- | A plain value, here till an (unescaped) space
value :: Parser String
value = many nonSpace

escaped :: Char -> Parser Char
escaped c = c <$ string ("\\" <> [c])

anyToken :: Parser Char
anyToken = satisfy $ const True

nonSpace :: Parser Char
nonSpace = escaped ' ' <|> satisfy (not . isSpace)

nonSpaceOr :: Char -> Parser Char
nonSpaceOr c = escaped ' ' <|> escaped c <|> satisfy (not . isSpaceOrChar)
  where
    isSpaceOrChar c'
        | isSpace c' = True
        | c' == c = True
        | otherwise = False
