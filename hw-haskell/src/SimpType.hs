module SimpType where

import           AlgebraicTerm

data SimpType = SElem String
              | SArrow SimpType SimpType
      deriving (Eq, Show)

stringOfSimpType :: SimpType -> String
stringOfSimpType (SElem s) = s
stringOfSimpType (SArrow tl tr) =
    "(" ++ stringOfSimpType tl ++ " -> " ++ stringOfSimpType tr ++ ")"

algTermOfSimpType :: SimpType -> AlgebraicTerm
algTermOfSimpType (SElem s) = Var s
algTermOfSimpType (SArrow tl tr) =
    Fun "a" [algTermOfSimpType tl, algTermOfSimpType tr]

simpTypeOfAlgTerm :: AlgebraicTerm -> SimpType
simpTypeOfAlgTerm (Var s) = SElem s
simpTypeOfAlgTerm (Fun n (tl : tr : [])) =
    if n == "a"
    then SArrow (simpTypeOfAlgTerm tl) (simpTypeOfAlgTerm tr)
    else error "incorrect algebraic term"
simpTypeOfAlgTerm _ = error "incorrect algebraic term"
