module Unify where

import           AlgebraicTerm

import           Data.List     (maximum)

--------------------------------------------------------------------------------

systemToSolution :: [(AlgebraicTerm, AlgebraicTerm)] -> (AlgebraicTerm, AlgebraicTerm)
systemToSolution l =
  let
    name = uniqueName l
  in
    (Fun name (map fst l), Fun name (map snd l))
  where
    uniqueName :: [(AlgebraicTerm, AlgebraicTerm)] -> String
    uniqueName l' = replicate (maximum (map getPairMaxLength l') + 1) 'f'

    getPairMaxLength :: (AlgebraicTerm, AlgebraicTerm) -> Int
    getPairMaxLength (x, y) = max (getMaxLength x) (getMaxLength y)

    getMaxLength :: AlgebraicTerm -> Int
    getMaxLength (Fun n l') = max (length n) (maximum $ map getMaxLength l')
    getMaxLength (Var n)    = length n

--------------------------------------------------------------------------------
