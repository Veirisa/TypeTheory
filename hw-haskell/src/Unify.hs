module Unify where

import           Types.AlgebraicTerm

import           Data.List           (maximum, partition, span)
import qualified Data.Map            as M (Map, fromList, lookup)

--------------------------------------------------------------------------------

-- Одно уравнение по списку уравнений
systemToEquation :: [(AlgebraicTerm, AlgebraicTerm)] -> (AlgebraicTerm, AlgebraicTerm)
systemToEquation l =
  let
    name = uniqueName l
  in
    (Fun name (map fst l), Fun name (map snd l))
  where
    getMaxLength :: AlgebraicTerm -> Int
    getMaxLength (Fun n l') = max (length n) (maximum $ map getMaxLength l')
    getMaxLength (Var x)    = length x

    getPairMaxLength :: (AlgebraicTerm, AlgebraicTerm) -> Int
    getPairMaxLength (left, right) = max (getMaxLength left) (getMaxLength right)

    uniqueName :: [(AlgebraicTerm, AlgebraicTerm)] -> String
    uniqueName l' = replicate (maximum (map getPairMaxLength l') + 1) 'f'

--------------------------------------------------------------------------------

-- Применение подстановки к уравнению
applySubstitution :: [(String, AlgebraicTerm)] -> AlgebraicTerm -> AlgebraicTerm
applySubstitution l = substitution (M.fromList l)
  where
    substitution :: M.Map String AlgebraicTerm -> AlgebraicTerm -> AlgebraicTerm
    substitution m (Fun n l) = Fun n (map (substitution m) l)
    substitution m at@(Var x) =
        case M.lookup x m of
            Just newAt -> newAt
            _          -> at

--------------------------------------------------------------------------------

-- Проверка решения
checkSolution :: [(String, AlgebraicTerm)] -> [(AlgebraicTerm, AlgebraicTerm)] -> Bool
checkSolution lSub lEq =
  let
    (left, right) = systemToEquation lEq
  in
    applySubstitution lSub left == applySubstitution lSub right

--------------------------------------------------------------------------------

-- Решение системы
solveSystem :: [(AlgebraicTerm, AlgebraicTerm)] -> Maybe [(String, AlgebraicTerm)]
solveSystem lEq =
    case solveGlobal $ map (\eq -> (eq, False)) lEq of
        Just result -> Just $ map convert result
        _           -> Nothing
  where
    convert :: ((AlgebraicTerm, AlgebraicTerm), Bool) -> (String, AlgebraicTerm)
    convert ((Var x, right), True) = (x, right)
    convert _                      = error "incorrect equation in solution"

    containsVar :: String -> AlgebraicTerm -> Bool
    containsVar x (Var y)   = x == y
    containsVar x (Fun _ l) = any (containsVar x) l

    trySubstitution :: String -> AlgebraicTerm -> AlgebraicTerm -> AlgebraicTerm
    trySubstitution x newAt at =
        if containsVar x at
        then applySubstitution [(x, newAt)] at
        else at

    substitution :: String -> AlgebraicTerm -> ((AlgebraicTerm, AlgebraicTerm), Bool)
                    -> ((AlgebraicTerm, AlgebraicTerm), Bool)
    substitution x newAt ((at1, at2), flag) =
      let
        left = trySubstitution x newAt at1
        right = trySubstitution x newAt at2
      in
        ((left, right), flag)

    solve :: [((AlgebraicTerm, AlgebraicTerm), Bool)]
             -> Maybe [((AlgebraicTerm, AlgebraicTerm), Bool)]
    solve (((f@(Fun _ _), v@(Var _)), _) : eqs) = solve $ ((v, f), False) : eqs
    solve (((Fun n1 l1, Fun n2 l2), _) : eqs) =
      let
        len = length l1
      in
        if n1 /= n2 || len /= length l2
        then Nothing
        else solveGlobal $ zip (zip l1 l2) (replicate len False) ++ eqs
    solve ((eq'@(Var x, at), _) : eqs) =
        if containsVar x at
        then Nothing
        else solveGlobal $ map (substitution x at) eqs ++ [(eq', True)]
    solve _ = Just []

    solveGlobal :: [((AlgebraicTerm, AlgebraicTerm), Bool)]
                   -> Maybe [((AlgebraicTerm, AlgebraicTerm), Bool)]
    solveGlobal lEq@((_, True) : _) = Just lEq
    solveGlobal lEq@(((at1, at2), _) : eqs) =
        if at1 == at2
        then solveGlobal eqs
        else solve lEq
    solveGlobal _ = Just []
