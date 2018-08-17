module Reduction where

import           Lambda

import           Data.Char (isLetter)
import qualified Data.Map  as M (Map, empty, findWithDefault, fromList, insert,
                                 lookup, member)
import qualified Data.Set  as S (Set, delete, empty, fromList, insert,
                                 intersection, member, null, singleton, toList,
                                 union)

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

getAllNames :: Lambda -> S.Set String
getAllNames (Var s)     = S.singleton s
getAllNames (Abs s l)   = S.union (S.singleton s) (getAllNames l)
getAllNames (App l1 l2) = S.union (getAllNames l1) (getAllNames l2)

newName :: S.Set String -> Int -> String -> String
newName block num s =
  let
    name = takeWhile isLetter s ++ show num
  in
    if S.member name block
    then newName block (num + 1) s
    else name

renameFailAbs :: S.Set String -> S.Set String -> M.Map String String -> Lambda
                 -> (Lambda, S.Set String)
renameFailAbs block fails m l@(Var s) =
    case M.lookup s m of
        Just newS -> (Var newS, block)
        _         -> (l, block)
renameFailAbs block fails m l@(Abs s' l') =
  let
    name = newName block 0 s'
    (newBlock1, newM) =
        if S.member s' fails
        then (S.insert name block, M.insert s' name m)
        else (block, m)
    (newL, newBlock2) = renameFailAbs newBlock1 fails newM l'
  in
    (Abs (M.findWithDefault s' s' newM) newL, newBlock2)
renameFailAbs block fails m l@(App l1 l2) =
  let
    (newL1, newBlock1) = renameFailAbs block fails m l1
    (newL2, newBlock2) = renameFailAbs newBlock1 fails m l2
  in
    (App newL1 newL2, newBlock2)

substitution :: S.Set String -> Lambda -> Lambda -> String -> (Lambda, S.Set String)
substitution block sl l x =
  let
    failVars = failsFreeToSubst sl l x
    (newL, newBlock) = renameFailAbs block failVars M.empty l
  in
    (freeSubstitution sl newL x, newBlock)
  where
    freeSubstitution :: Lambda -> Lambda -> String -> Lambda
    freeSubstitution sl l@(Var s) x = if s == x then sl else l
    freeSubstitution sl l@(Abs s' l') x =
        if s' == x then l else Abs s' (freeSubstitution sl l' x)
    freeSubstitution sl (App l1 l2) x =
        App (freeSubstitution sl l1 x) (freeSubstitution sl l2 x)

reduction :: S.Set String -> Lambda -> (Lambda, Bool)
reduction block (App (Abs s1' l1') l2) =
  let
    (newL, _) = substitution block l2 l1' s1'
  in
    (newL, True)
reduction block l@(App l1 l2) =
    case reduction block l1 of
        (newL1, True) -> (App newL1 l2, True)
        _       ->
            case reduction block l2 of
                (newL2, True) -> (App l1 newL2, True)
                _             -> (l, False)
reduction block l@(Abs s' l') =
    case reduction block l' of
        (newL', True) -> (Abs s' newL', True)
        _             -> (l, False)
reduction block l@(Var s) = (l, False)

-- Выполнение одного шага бета-редукции с использованием нормального порядка
normalBetaReduction :: Lambda -> Lambda
normalBetaReduction l = fst $ reduction (getAllNames l) l

--------------------------------------------------------------------------------

-- Неэффективное сведение выражения к нормальной форме с использованием
-- нормального порядка
easyReduceToNormalForm :: Lambda -> Lambda
easyReduceToNormalForm l =
    case reduction (getAllNames l) l of
        (newL, True) -> easyReduceToNormalForm newL
        _            -> l

getFailAbsNames :: Lambda -> S.Set String
getFailAbsNames l = S.intersection (getVarNames l) (getAbsNames l)

getVarNames :: Lambda -> S.Set String
getVarNames (Var s)     = S.singleton s
getVarNames (Abs s l)   = getVarNames l
getVarNames (App l1 l2) = S.union (getVarNames l1) (getVarNames l2)

getAbsNames :: Lambda -> S.Set String
getAbsNames (Var s)     = S.empty
getAbsNames (Abs s l)   = S.union (S.singleton s) (getAbsNames l)
getAbsNames (App l1 l2) = S.union (getAbsNames l1) (getAbsNames l2)

type MapStoLB = M.Map String (Lambda, Bool)

smartReduction :: S.Set String -> MapStoLB -> Bool -> Lambda
                  -> (Lambda, Bool, S.Set String, MapStoLB)

smartReduction block mL isLeft (App (Abs s1' l1') l2) =
  let
    failVars = failsFreeToSubst l2 l1' s1'
    (newL, newBlock) = renameFailAbs block failVars M.empty l1'
  in
    (newL, True, newBlock, M.insert s1' (l2, False) mL)

smartReduction block mL isLeft l@(App l1 l2) =
    case smartReduction block mL True l1 of
        (newL1, True, newBlock, newML) -> (App newL1 l2, True, newBlock, newML)
        _                              ->
            case smartReduction block mL False l2 of
                (newL2, True, newBlock, newML) -> (App l1 newL2, True, newBlock, newML)
                _                              -> (l, False, block, mL)

smartReduction block mL isLeft l@(Abs s' l') =
    case smartReduction block mL False l' of
        (newL', True, newBlock, newML) -> (Abs s' newL', True, newBlock, newML)
        _                              -> (l, False, block, mL)

smartReduction block mL isLeft l@(Var s) =
    case (M.lookup s mL, isLeft) of
        -- мы - левый ребенок аппликации и при этом абстракция - просто return
        -- кладем: ничего, ибо ничего в лямбде не поменяли
        (Just (realL@(Abs _ _), False), True) ->
          let
            (unRealL, unBlock) = renameFailAbs block (getAbsNames realL) M.empty realL
          in
            (unRealL, True, unBlock, mL)

        -- мы - левый ребенок аппликации, редуцируем пока не станем Abs или норм формой
        (Just (realL, False), True) ->
            case doSmartReductionToAbs block mL True realL of
                -- мы доредуцировали до норм формы - return
                -- кдадем: Лямбда + True
                (newL, False, newBlock, newML) ->
                  let
                    (unNewL, unNewBlock) = renameFailAbs newBlock (getAbsNames newL) M.empty newL
                  in
                    (unNewL, True, unNewBlock, M.insert s (newL, True) newML)
                -- мы доредуцировали до Abs - return
                -- кладем: Лямбда + False, ибо не факт, что мы в норм форме
                (newL, _, newBlock, newML) ->
                  let
                    (unNewL, unNewBlock) = renameFailAbs newBlock (getAbsNames newL) M.empty newL
                  in
                    (unNewL, True, unNewBlock, M.insert s (newL, False) newML)

        -- можем спокойно редуцировать до норм формы
        -- кладем: Лямбда + True
        (Just (realL, False), False) ->
          let
            (newL, _, newBlock, newML) = doSmartReductionToNorm block mL False realL
            (unNewL, unNewBlock) = renameFailAbs newBlock (getAbsNames newL) M.empty newL
          in
            (unNewL, True, unNewBlock, M.insert s (newL, True) newML)

        -- наша прошлая копия уже доредуцировала - просто return
        -- кладем: ничего, ибо и так лежит лямбда в норм форме
        (Just (realL, True), _) ->
          let
            (unRealL, unBlock) = renameFailAbs block (getAbsNames realL) M.empty realL
          in
            (unRealL, True, unBlock, mL)

        -- эта переменная не является лениво спрятанной лямбдой - просто return
        _ -> (l, False, block, mL)

doSmartReductionToAbs :: S.Set String -> MapStoLB -> Bool -> Lambda
                   -> (Lambda, Bool, S.Set String, MapStoLB)
doSmartReductionToAbs block mL isLeft l' =
    case smartReduction block mL isLeft l' of
        res@(Abs _ _, _, _, _)      -> res
        res@(_, False, _, _)        -> res
        (newL, _, newBlock, newML)  -> doSmartReductionToAbs newBlock newML isLeft newL

doSmartReductionToNorm :: S.Set String -> MapStoLB -> Bool -> Lambda
                   -> (Lambda, Bool, S.Set String, MapStoLB)
doSmartReductionToNorm block mL isLeft l' =
    case smartReduction block mL isLeft l' of
        res@(_, False, _, _)        -> res
        (newL, _, newBlock, newML)  -> doSmartReductionToNorm newBlock newML isLeft newL

reduceToNormalForm :: Lambda -> Lambda
reduceToNormalForm l =
  let
    (uniqueL, allNames) = renameFailAbs (getAllNames l) (getFailAbsNames l) M.empty l
    (normL, _, _,  _) = doSmartReductionToNorm allNames M.empty False uniqueL
  in
    normL

{-
root: ((\x2.(x2 x)) (\x3.(x3 x)))
in Beta-Redex: ((\x2.(x2 x)) (\x3.(x3 x)))
out Beta-Redex: (x2 x), True
root: ((\x2.(x2 x)) (\x3.(x3 x)))
in App: (x2 x)
Var: x2
in Beta-Redex: ((\x3.(x3 x)) x)
out Beta-Redex: (x3 x), True
(x3 x)
-}

-- (\\x.x) y
-- "((\\y.((\\x.(x y)) (\\x.(x y)))) x)"
-- (\\x.(\\y.(y x)) (\\z.(z x))) p
-- "(\\x.x x x) (\\y.y)"
