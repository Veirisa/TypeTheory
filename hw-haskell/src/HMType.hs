module HMType where

import           AlgebraicTerm

data HMType = HMElem String
            | HMArrow HMType HMType
            | HMForAll String HMType
      deriving (Eq, Show)

stringOfHMType :: HMType -> String
stringOfHMType (HMElem x) = x
stringOfHMType (HMArrow l r) =
    "(" ++ stringOfHMType l ++ " -> " ++ stringOfHMType r ++ ")"
stringOfHMType (HMForAll x m) = "V" ++ x ++ "." ++ stringOfHMType m

algTermOfHMType :: HMType -> AlgebraicTerm
algTermOfHMType (HMElem x) = Var x
algTermOfHMType (HMArrow l r) =
    Fun "a" [algTermOfHMType l, algTermOfHMType r]
algTermOfHMType (HMForAll x m) =
    error "HMType with forAll can not be converted into AlgebraicTerm"

hmTypeOfAlgTerm :: AlgebraicTerm -> HMType
hmTypeOfAlgTerm (Var x) = HMElem x
hmTypeOfAlgTerm (Fun n (l : r : [])) =
    if n == "a"
    then HMArrow (hmTypeOfAlgTerm l) (hmTypeOfAlgTerm r)
    else error "incorrect algebraic term"
hmTypeOfAlgTerm _ = error "incorrect algebraic term"
