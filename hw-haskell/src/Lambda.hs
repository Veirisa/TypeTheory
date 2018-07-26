module Lambda where

import           Common

import           Text.Megaparsec

--------------------------------------------------------------------------------

data Lambda = Var String
            | Abs String Lambda
            | App Lambda Lambda
    deriving (Eq, Show)

stringOfLambda :: Lambda -> String
stringOfLambda (Var s)     = s
stringOfLambda (Abs s l)   = "(\\" ++ s ++ "." ++ stringOfLambda l ++ ")"
stringOfLambda (App l1 l2) = "(" ++ stringOfLambda l1 ++ " " ++ stringOfLambda l2 ++ ")"

lambdaOfString :: String -> Lambda
lambdaOfString s = case runParser parserLambda "" s of
    Left _       -> Var ""
    Right lambda -> lambda
  where
    parserVar :: Parser Lambda
    parserVar = do
        name <- identifier
        return $ Var name

    parserAbs :: Parser Lambda
    parserAbs = do
        symbol "\\"
        name <- identifier
        symbol "."
        l <- parserLambda
        return $ Abs name l

    parserSubLambda :: Parser Lambda
    parserSubLambda = parens parserLambda <|> parserVar <|> parserAbs

    parserLambda :: Parser Lambda
    parserLambda = do
        l <- parserSubLambda
        ls <- many parserApp
        return $ seqApps (reverse (l : ls))
      where
        parserApp :: Parser Lambda
        parserApp = do
            symbol " "
            parserSubLambda

        seqApps :: [Lambda] -> Lambda
        seqApps (l' : [])  = l'
        seqApps (l' : ls') = App (seqApps ls') l'
        seqApps _          = error "impossible situation"
