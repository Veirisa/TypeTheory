module SimpType where

import           AlgebraicTerm

data SimpType = SElem String
              | SArrow SimpType SimpType
      deriving (Eq, Show)

stringOfSimpType :: SimpType -> String
stringOfSimpType (SElem x) = x
stringOfSimpType (SArrow l r) =
    "(" ++ stringOfSimpType l ++ " -> " ++ stringOfSimpType r ++ ")"

algTermOfSimpType :: SimpType -> AlgebraicTerm
algTermOfSimpType (SElem x) = Var x
algTermOfSimpType (SArrow l r) =
    Fun "a" [algTermOfSimpType l, algTermOfSimpType r]

simpTypeOfAlgTerm :: AlgebraicTerm -> SimpType
simpTypeOfAlgTerm (Var x) = SElem x
simpTypeOfAlgTerm (Fun n (l : r : [])) =
    if n == "a"
    then SArrow (simpTypeOfAlgTerm l) (simpTypeOfAlgTerm r)
    else error "incorrect algebraic term"
simpTypeOfAlgTerm _ = error "incorrect algebraic term"
