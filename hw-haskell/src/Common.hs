module Common where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space empty empty empty

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = try (name >>= isNotKeyword)
  where
    name :: Parser String
    name = (:) <$> letterChar <*> takeWhileP Nothing isNameSym

    isNotKeyword :: String -> Parser String
    isNotKeyword w =
        if elem w keywordList
        then fail $ "keyword " ++ show w ++ " can't be a name"
        else return w

    keywordList :: [String]
    keywordList = ["let", "in"]

    isNameSym :: Char -> Bool
    isNameSym ch = notElem ch blocked

    blocked :: [Char]
    blocked = ['\\', ' ', '.', '(', ')']

keyword :: String -> Parser ()
keyword w = try (string w *> notFollowedBy alphaNumChar)
