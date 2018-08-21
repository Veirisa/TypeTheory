module HMType where

import           AlgebraicTerm

data HMType = HMElem String
            | HMArrow HMType HMType
            | HMForAll String HMType
      deriving (Eq, Show)

stringOfHMType :: HMType -> String
stringOfHMType (HMElem s) = s
stringOfHMType (HMArrow tl tr) =
    "(" ++ stringOfHMType tl ++ " -> " ++ stringOfHMType tr ++ ")"
stringOfHMType (HMForAll s t) = "V" ++ s ++ "." ++ stringOfHMType t

algTermOfHMType :: HMType -> AlgebraicTerm
algTermOfHMType (HMElem s) = Var s
algTermOfHMType (HMArrow tl tr) =
    Fun "a" [algTermOfHMType tl, algTermOfHMType tr]
algTermOfHMType _ =
    error "HMType with forAll can not be converted into AlgebraicTerm"

hmTypeOfAlgTerm :: AlgebraicTerm -> HMType
hmTypeOfAlgTerm (Var s) = HMElem s
hmTypeOfAlgTerm (Fun n (tl : tr : [])) =
    if n == "a"
    then HMArrow (hmTypeOfAlgTerm tl) (hmTypeOfAlgTerm tr)
    else error "incorrect algebraic term"
hmTypeOfAlgTerm _ = error "incorrect algebraic term"
