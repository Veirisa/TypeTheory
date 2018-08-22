module HMLambda where

import           Common

import           Text.Megaparsec

data HMLambda = HMVar String
              | HMAbs String HMLambda
              | HMApp HMLambda HMLambda
              | HMLet String HMLambda HMLambda
    deriving (Eq, Show)

stringOfHMLambda :: HMLambda -> String
stringOfHMLambda (HMVar s)     = s
stringOfHMLambda (HMAbs s l)   = "(\\" ++ s ++ "." ++ stringOfHMLambda l ++ ")"
stringOfHMLambda (HMApp l1 l2) = "(" ++ stringOfHMLambda l1 ++ " " ++ stringOfHMLambda l2 ++ ")"
stringOfHMLambda (HMLet s l1 l2) =
  let
    sl1 = stringOfHMLambda l1
    sl2 = stringOfHMLambda l2
    fullSl1 = if isApp l1 then "(" ++ sl1 ++ ")" else sl1
    fullSl2 = if isApp l2 then "(" ++ sl2 ++ ")" else sl2
  in
    "(let " ++ s ++ " = " ++ fullSl1 ++ " in " ++ fullSl2 ++ ")"
  where
    isApp :: HMLambda -> Bool
    isApp (HMApp _ _) = True
    isApp _           = False

hmLambdaOfString :: String -> HMLambda
hmLambdaOfString s = case runParser parserLambda "" s of
    Left _       -> HMVar ""
    Right lambda -> lambda
  where
    parserVar :: Parser HMLambda
    parserVar = do
        name <- identifier
        return $ HMVar name

    parserAbs :: Parser HMLambda
    parserAbs = do
        symbol "\\"
        name <- identifier
        symbol "."
        l <- parserLambda
        return $ HMAbs name l

    parserLet :: Parser HMLambda
    parserLet = do
        keyword "let"
        symbol " "
        name <- identifier
        symbol " "
        symbol "="
        symbol " "
        l1 <- parserSubLambda
        symbol " "
        keyword "in"
        symbol " "
        l2 <- parserSubLambda
        return $ HMLet name l1 l2

    parserSubLambda :: Parser HMLambda
    parserSubLambda = parens parserLambda <|> parserVar <|> parserAbs <|> parserLet

    parserLambda :: Parser HMLambda
    parserLambda = do
        l <- parserSubLambda
        ls <- many parserApp
        return $ seqApps (reverse (l : ls))
      where
        parserApp :: Parser HMLambda
        parserApp = do
            symbol " "
            parserSubLambda

        seqApps :: [HMLambda] -> HMLambda
        seqApps (l' : [])  = l'
        seqApps (l' : ls') = HMApp (seqApps ls') l'
        seqApps _          = error "impossible situation"
