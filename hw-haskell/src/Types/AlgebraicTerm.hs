module Types.AlgebraicTerm where

import           Types.Common

import           Text.Megaparsec

data AlgebraicTerm = Var String
                   | Fun String [AlgebraicTerm]
    deriving (Eq, Show)

stringOfAlgTerm :: AlgebraicTerm -> String
stringOfAlgTerm = convert False
  where
    convert :: Bool -> AlgebraicTerm -> String
    convert True at        = ' ' : convert False at
    convert _ (Fun name l) = "(" ++ name ++ concatMap (convert True) l ++ ")"
    convert _ (Var name)   = name

algTermOfString :: String -> AlgebraicTerm
algTermOfString s = case runParser parserAlgTerm "" s of
    Left _     -> Var ""
    Right term -> term
  where
    parserVar :: Parser AlgebraicTerm
    parserVar = do
        name <- identifier
        return $ Var name

    parserFun :: Parser AlgebraicTerm
    parserFun = do
        name <- identifier
        args <- many parserFunArg
        return $ Fun name args
      where
        parserFunArg :: Parser AlgebraicTerm
        parserFunArg = do
            symbol " "
            parserAlgTerm

    parserAlgTerm :: Parser AlgebraicTerm
    parserAlgTerm = parens parserFun <|> parserVar
