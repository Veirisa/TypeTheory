module Reduction where

import           Lambda

import           Data.Char (isLetter)
import qualified Data.Map  as M (Map, empty, findWithDefault, fromList, insert,
                                 lookup, member)
import qualified Data.Set  as S (Set, delete, empty, fromList, insert, member,
                                 null, toList, union)

--------------------------------------------------------------------------------

failsFreeToSubst :: Lambda -> Lambda -> String -> S.Set String
failsFreeToSubst sl l x  = snd $ getFailsFreeToSubst (S.fromList (freeVars sl)) l x
  where
    getFailsFreeToSubst :: S.Set String -> Lambda -> String -> (Bool, S.Set String)
    getFailsFreeToSubst set (Var s) x = (s == x, S.empty)
    getFailsFreeToSubst set (Abs s l) x =
      let
        (was, fails) = getFailsFreeToSubst set l x
        haveNewFail = was && S.member s set
      in
        (was, if haveNewFail then S.insert s fails else fails)
    getFailsFreeToSubst set (App l1 l2) x =
      let
        (was1, fails1) = getFailsFreeToSubst set l1 x
        (was2, fails2) = getFailsFreeToSubst set l2 x
      in
        (was1 || was2, S.union fails1 fails2)

-- Проверка свободы для подстановки: что -> где -> вместо чего -> ?
freeToSubst :: Lambda -> Lambda -> String -> Bool
freeToSubst sl l = S.null . failsFreeToSubst sl l

--------------------------------------------------------------------------------

freeVarsSet :: Lambda -> S.Set String
freeVarsSet (Var s)     = S.fromList [s]
freeVarsSet (Abs s l)   = S.delete s (freeVarsSet l)
freeVarsSet (App l1 l2) = S.union (freeVarsSet l1) (freeVarsSet l2)

-- Список имен свободных переменных
freeVars :: Lambda -> [String]
freeVars = S.toList . freeVarsSet

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
isAlphaEquivalent = checkAlphaEq 0 M.empty M.empty
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

newName :: Int -> String -> String
newName num s = takeWhile isLetter s ++ show num

substitution :: Int -> Lambda -> Lambda -> String -> Lambda
substitution num sl l x =
  let
    failVars = failsFreeToSubst sl l x
    newL = renameFailFreeVars num failVars M.empty l
  in
    freeSubstitution sl newL x
  where
    renameFailFreeVars :: Int -> S.Set String -> M.Map String String -> Lambda -> Lambda
    renameFailFreeVars num fails m l@(Var s) =
        case M.lookup s m of
            Just newS -> Var newS
            _         -> l
    renameFailFreeVars num fails m l@(Abs s' l') =
      let
        newM =
            if S.member s' fails && not (M.member s' m)
            then M.insert s'(newName num s') m
            else m
      in
        Abs (M.findWithDefault s' s' newM) (renameFailFreeVars num fails newM l')
    renameFailFreeVars num fails m l@(App l1 l2) =
        App (renameFailFreeVars num fails m l1) (renameFailFreeVars num fails m l2)

    freeSubstitution :: Lambda -> Lambda -> String -> Lambda
    freeSubstitution sl l@(Var s) x = if s == x then sl else l
    freeSubstitution sl l@(Abs s' l') x =
        if s' == x then l else Abs s' (freeSubstitution sl l' x)
    freeSubstitution sl (App l1 l2) x =
        App (freeSubstitution sl l1 x) (freeSubstitution sl l2 x)

reduction :: Int -> Lambda -> (Lambda, Bool)
reduction num (App (Abs s1' l1') l2) = (substitution num l2 l1' s1', True)
reduction num l@(App l1 l2) =
  case reduction num l1 of
      (newL1, True) -> (App newL1 l2, True)
      _ ->
          case reduction num l2 of
              (newL2, True) -> (App l1 newL2, True)
              _             -> (l, False)
reduction num l@(Abs s' l') =
  case reduction num l' of
      (newL', True) -> (Abs s' newL', True)
      _             -> (l, False)
reduction num l@(Var s) = (l, False)

-- Выполнение одного шага бета-редукции с использованием нормального порядка
normalBetaReduction :: Lambda -> Lambda
normalBetaReduction = fst . reduction 0

--------------------------------------------------------------------------------

reduceToNormalForm :: Lambda -> Lambda
reduceToNormalForm = doReduction 0
  where
    doReduction :: Int -> Lambda -> Lambda
    doReduction num l =
        case reduction num l of
            (newL, True) -> doReduction (num + 1) newL
            _            -> l
