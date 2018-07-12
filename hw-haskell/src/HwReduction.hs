module HwReduction where

import qualified Data.Map as M (Map, findWithDefault, fromList, insert)
import qualified Data.Set as S (Set, delete, fromList, insert, member, toList,
                                union)
import           Hw

--------------------------------------------------------------------------------

-- Проверка свободы для подстановки: что -> где -> вместо чего -> ?
freeToSubst :: Lambda -> Lambda -> String -> Bool
freeToSubst sl l x = snd $ checkFreeToSubst (S.fromList (freeVars sl)) l x
  where
    checkFreeToSubst :: S.Set String -> Lambda -> String -> (Bool, Bool)
    checkFreeToSubst set (Var s) x = (s == x, True)
    checkFreeToSubst set (Abs s l) x =
      let
        (was, ans) = checkFreeToSubst set l x
        newAns = ans && not (was && S.member s set)
      in
        (was, newAns)
    checkFreeToSubst set (App l1 l2) x =
      let
        (was1, ans1) = checkFreeToSubst set l1 x
        (was2, ans2) = checkFreeToSubst set l2 x
      in
        (was1 || was2, ans1 && ans2)

--------------------------------------------------------------------------------

-- Список имен свободных переменных
freeVars :: Lambda -> [String]
freeVars = S.toList . freeVarsSet (S.fromList [])
  where
    freeVarsSet :: S.Set String -> Lambda -> S.Set String
    freeVarsSet set (Var s) = S.insert s set
    freeVarsSet set (Abs s l) = S.delete s (freeVarsSet set l)
    freeVarsSet set (App l1 l2) = S.union (freeVarsSet set l1) (freeVarsSet set l2)

--------------------------------------------------------------------------------

-- Проверка, находится ли лямбда-выражение в нормальной форме
isNormalForm :: Lambda -> Bool
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App l1 l2)       = isNormalForm l1 && isNormalForm l2
isNormalForm (Abs s l)         = isNormalForm l
isNormalForm _                 = True

--------------------------------------------------------------------------------

-- Проверка, являются ли лямбда-выражения альфа-эквивалентными
isAlphaEquivalent :: Lambda -> Lambda -> Bool
isAlphaEquivalent = checkAlphaEq 0 (M.fromList []) (M.fromList [])
  where
    checkAlphaEq :: Int -> M.Map String String -> M.Map String String -> Lambda -> Lambda -> Bool
    checkAlphaEq num m1 m2 (Var s1) (Var s2) =
        M.findWithDefault s1 s1 m1 == M.findWithDefault s2 s2 m2
    checkAlphaEq num m1 m2 (Abs s1 l1) (Abs s2 l2) =
      let
        newS = show num
        newNum = num + 1
        newM1 = M.insert s1 newS m1
        newM2 = M.insert s2 newS m2
      in
        checkAlphaEq newNum newM1 newM2 l1 l2
    checkAlphaEq num m1 m2 (App l1 l1') (App l2 l2') =
        checkAlphaEq num m1 m2 l1 l2 && checkAlphaEq num m1 m2 l1' l2'
    checkAlphaEq _ _ _ _ _ = False

--------------------------------------------------------------------------------

substitution :: Lambda -> Lambda -> String -> Lambda
substitution sl l@(Var s) x = if s == x then sl else l
substitution sl l@(Abs s' l') x =
    if s' == x then l else Abs s' (substitution sl l' x)
substitution sl (App l1 l2) x =
    App (substitution sl l1 x) (substitution sl l2 x)

reduction :: Lambda -> (Lambda, Bool)
reduction (App (Abs s1' l1') l2) = (substitution l2 l1' s1', True)
reduction l@(App l1 l2) =
  case reduction l1 of
      (newL1, True) -> (App newL1 l2, True)
      _ ->
          case reduction l2 of
              (newL2, True) -> (App l1 newL2, True)
              _             -> (l, False)
reduction l@(Abs s' l') =
  case reduction l' of
      (newL', True) -> (Abs s' newL', True)
      _             -> (l, False)
reduction l@(Var s) = (l, False)

-- Выполнение одного шага бета-редукции с использованием нормального порядка
normalBetaReduction :: Lambda -> Lambda
normalBetaReduction = fst . reduction

--------------------------------------------------------------------------------

-- Сведение выражения к нормальной форме с использованием нормального порядка редукции
reduceToNormalForm :: Lambda -> Lambda
reduceToNormalForm = undefined
