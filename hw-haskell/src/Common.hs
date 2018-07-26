module Common where

import           Control.Monad              (void)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space empty empty empty

symbol :: String -> Parser ()
symbol = void . L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (:) <$> letterChar <*> takeWhileP Nothing isNameSym
  where
    isNameSym :: Char -> Bool
    isNameSym ch = not (elem ch blocked)

    blocked :: [Char]
    blocked = ['\\', ' ', '.', '(', ')']
