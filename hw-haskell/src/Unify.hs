module Unify where

import           AlgebraicTerm

import           Data.List     (maximum, null, partition)
import qualified Data.Map      as M (Map, empty, fromList, insert, lookup,
                                     toList)
import           Data.Maybe    (isNothing)

--------------------------------------------------------------------------------

-- Одно уравнение по списку уравнений
systemToEquation :: [(AlgebraicTerm, AlgebraicTerm)] -> (AlgebraicTerm, AlgebraicTerm)
systemToEquation l =
  let
    name = uniqueName l
  in
    (Fun name (map fst l), Fun name (map snd l))
  where
    uniqueName :: [(AlgebraicTerm, AlgebraicTerm)] -> String
    uniqueName l' = replicate (maximum (map getPairMaxLength l') + 1) 'f'

    getPairMaxLength :: (AlgebraicTerm, AlgebraicTerm) -> Int
    getPairMaxLength (left, right) = max (getMaxLength left) (getMaxLength right)

    getMaxLength :: AlgebraicTerm -> Int
    getMaxLength (Fun n l') = max (length n) (maximum $ map getMaxLength l')
    getMaxLength (Var x)    = length x

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

-- Решение системы (Nothing - если его не существует)
solveSystem :: [(AlgebraicTerm, AlgebraicTerm)] -> Maybe [(String, AlgebraicTerm)]
solveSystem lEq =
    case solveGlobal $ map (\eq -> (eq, False)) lEq of
        Just result -> Just $ map convert result
        _           -> Nothing
  where
    convert :: ((AlgebraicTerm, AlgebraicTerm), Bool) -> (String, AlgebraicTerm)
    convert ((Var x, right), True) = (x, right)
    convert _                      = error "incorrect situation"

    containsVar :: String -> AlgebraicTerm -> Bool
    containsVar x (Var y)   = x == y
    containsVar x (Fun n l) = any (containsVar x) l

    substitution :: [(String, AlgebraicTerm)] -> ((AlgebraicTerm, AlgebraicTerm), Bool)
                    -> ((AlgebraicTerm, AlgebraicTerm), Bool)
    substitution lSub ((at1, at2), _) =
        ((applySubstitution lSub at1, applySubstitution lSub at2), False)

    solveGlobal :: [((AlgebraicTerm, AlgebraicTerm), Bool)]
                   -> Maybe [((AlgebraicTerm, AlgebraicTerm), Bool)]
    solveGlobal lEq@((_, True) : _) = Just lEq
    solveGlobal lEq@(((at1, at2), _) : lEqs) =
        if at1 == at2
        then solveGlobal lEqs
        else solve lEq
    solveGlobal _ = Just []

    solve :: [((AlgebraicTerm, AlgebraicTerm), Bool)]
             -> Maybe [((AlgebraicTerm, AlgebraicTerm), Bool)]
    solve (((f@(Fun _ _), v@(Var _)), _) : lEqs) = solve $ ((v, f), False) : lEqs
    solve (((Fun n1 l1, Fun n2 l2), _) : lEqs) =
      let
        len = length l1
      in
        if n1 /= n2 || len /= length l2
        then Nothing
        else solveGlobal $ (zip (zip l1 l2) (replicate len False)) ++ lEqs
    solve ((eq@(Var x, at), _) : lEqs) =
      let
        (have, notHave) =
            partition (\((at1, at2), _) -> containsVar x at1 || containsVar x at2) lEqs
      in
        if containsVar x at
        then Nothing
        else solveGlobal $ map (substitution [(x, at)]) have ++ notHave ++ [(eq, True)]
    solve _ = Just []
