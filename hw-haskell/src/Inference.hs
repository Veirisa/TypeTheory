module Inference where

import           HMLambda
import           HMType
import           Lambda
import           SimpType
import           Unify

import           Data.List ((\\))
import qualified Data.Map  as M (Map, delete, empty, findWithDefault, fromList,
                                 insert, lookup, map, singleton, toList, union)
import qualified Data.Set  as S (Set, delete, difference, empty, member,
                                 notMember, singleton, toList, union)

--------------------------------------------------------------------------------

-- Вывод типа в просто типизированном лямбда-исчислении
inferSimpType :: Lambda -> Maybe ([(String, SimpType)], SimpType)
inferSimpType lam =
  let
    (syst, st, _) = createSystem M.empty 0 lam
  in
    case solveSystem $ map (\(st1, st2) -> (algTermOfSimpType st1, algTermOfSimpType st2)) syst of
        Just solut ->
            Just $ (map (fmap simpTypeOfAlgTerm) solut,
                    simpTypeOfAlgTerm $ applySubstitution solut (algTermOfSimpType st))
        _ -> Nothing
  where
    genSimpType :: Int -> SimpType
    genSimpType num = SElem $ 't' : show num

    createSystem :: M.Map String SimpType -> Int -> Lambda
                    -> ([(SimpType, SimpType)], SimpType, M.Map String SimpType)
    createSystem m num (Var s) =
        case M.lookup s m of
            Just stv -> ([], stv, m)
            _        ->
              let
                newSt = genSimpType num
              in
                ([], newSt, M.insert s newSt m)
    createSystem m num (App l1 l2) =
      let
        (syst1, st1, m1) = createSystem m (2 * num + 1) l1
        (syst2, st2, m2) = createSystem m1 (2 * num + 2) l2
        newSt = genSimpType num
        newEq = (st1, SArrow st2 newSt)
      in
        (newEq : syst1 ++ syst2, newSt, M.union m1 m2)
    createSystem m num (Abs s l) =
      let
        (syst, st, m') = createSystem m (2 * num + 1) l
      in
        case M.lookup s m' of
            Just stv -> (syst, SArrow stv st, m')
            _ ->
              let
                newSt = genSimpType num
              in
                (syst, SArrow newSt st, M.insert s newSt m')

--------------------------------------------------------------------------------

-- Генерация новых типов в алгоритме W
genHMType :: Int -> HMType
genHMType num = HMElem $ 't' : show num

-- Начальный контекст - Мапа из свободных переменных в их типы
hmLambdaFreeVarsMap :: HMLambda -> M.Map String HMType
hmLambdaFreeVarsMap = genHMLambdaFreeVarsMap 0
  where
    genFreeVarType :: Int -> HMType
    genFreeVarType num = HMElem $ "fvt" ++ show num

    genHMLambdaFreeVarsMap :: Int -> HMLambda -> M.Map String HMType
    genHMLambdaFreeVarsMap num (HMVar s) = M.singleton s (genFreeVarType num)
    genHMLambdaFreeVarsMap num (HMApp l1 l2) =
      let
        m1 = genHMLambdaFreeVarsMap (2 * num + 1) l1
        m2 = genHMLambdaFreeVarsMap (2 * num + 2) l2
      in
        M.union m1 m2
    genHMLambdaFreeVarsMap num (HMAbs s l) =
        M.delete s (genHMLambdaFreeVarsMap (2 * num + 1) l)
    genHMLambdaFreeVarsMap num (HMLet s l1 l2) =
      let
        m1 = genHMLambdaFreeVarsMap (2 * num + 1) l1
        m2 = M.delete s (genHMLambdaFreeVarsMap (2 * num + 2) l2)
      in
        M.union m1 m2

-- Применение подстановки к типу (передавать мапу!)
applyTypeSubstitution :: M.Map String HMType -> HMType -> HMType
applyTypeSubstitution m tCur@(HMElem s) = M.findWithDefault tCur s m
applyTypeSubstitution m (HMArrow tl tr) =
    HMArrow (applyTypeSubstitution m tl) (applyTypeSubstitution m tr)
applyTypeSubstitution m (HMForAll _ t) = applyTypeSubstitution m t

-- Применение подстановки к нескольким типам (передавать мапу!)
applyMultiTypesSubstitution :: M.Map String HMType -> M.Map String HMType -> M.Map String HMType
applyMultiTypesSubstitution subst = M.map (applyTypeSubstitution subst)

-- Замена всех зависимых типовых переменных в типе на новые
-- (!!!) Возвращаю пустой мап для удобства в W
dependTypesSubstitution :: Int -> HMType -> (M.Map String HMType, HMType, Int)
dependTypesSubstitution num t =
  let
    dependVars = hmTypeDependVars t
    (newVarTypesMap, newNum) = genVarTypeMap num dependVars
  in
    (M.empty, applyTypeSubstitution newVarTypesMap t, newNum)
  where
    hmTypeDependVars :: HMType -> [String]
    hmTypeDependVars (HMForAll s t) = s : (hmTypeDependVars t)
    hmTypeDependVars _              = []

    genVarTypeMap :: Int -> [String] -> (M.Map String HMType, Int)
    genVarTypeMap num [] = (M.empty, num)
    genVarTypeMap num (x : xs) =
      let
        (newM, newNum) = genVarTypeMap (num + 1) xs
      in
        (M.insert x (genHMType num) newM, newNum)

-- Замыкание всех несвязанных типовых переменных в контексте по типу
closure :: M.Map String HMType -> HMType -> HMType
closure cont t =
  let
    contFreeVars = S.toList $ hmContextTypeFreeVarsSet (map snd $ M.toList cont)
    tFreeVars = S.toList $ hmTypeFreeVarsSet t
  in
    addDependence t (tFreeVars \\ contFreeVars)
  where
    hmContextTypeFreeVarsSet :: [HMType] -> S.Set String
    hmContextTypeFreeVarsSet [] = S.empty
    hmContextTypeFreeVarsSet (t : ts) =
        S.union (hmContextTypeFreeVarsSet ts) (hmTypeFreeVarsSet t)

    hmTypeFreeVarsSet :: HMType -> S.Set String
    hmTypeFreeVarsSet (HMElem s)        = S.singleton s
    hmTypeFreeVarsSet (HMArrow tl tr)   =
        S.union (hmTypeFreeVarsSet tl) (hmTypeFreeVarsSet tr)
    hmTypeFreeVarsSet (HMForAll s t)    = S.delete s (hmTypeFreeVarsSet t)

    addDependence :: HMType -> [String] -> HMType
    addDependence t []       = t
    addDependence t (x : xs) = HMForAll x (addDependence t xs)

-- Композиция подстановок
compose :: M.Map String HMType -> M.Map String HMType -> M.Map String HMType
compose next prev = M.union (applyMultiTypesSubstitution next prev) next

-- Создание и решение системы в алгоритме W
typeUnify :: HMType -> HMType -> Maybe (M.Map String HMType)
typeUnify left right =
    case solveSystem [(algTermOfHMType left, algTermOfHMType right)] of
        Just solut -> Just $ M.fromList (map (fmap hmTypeOfAlgTerm) solut)
        _          -> Nothing


-- Вывода типа в системе Хиндли-Милнера
algorithmW :: HMLambda -> Maybe ([(String, HMType)], HMType)
algorithmW l =
    case doAlgorithmW 0 (hmLambdaFreeVarsMap l) l of
        Nothing               -> Nothing
        Just (substMap, t, _) -> Just $ (M.toList substMap, t)
  where
    doAlgorithmW :: Int -> M.Map String HMType -> HMLambda
                    -> Maybe (M.Map String HMType, HMType, Int)
    doAlgorithmW num cont (HMVar s) =
        case M.lookup s cont of
            Nothing    -> Nothing
            Just sType -> Just $ dependTypesSubstitution num sType
    doAlgorithmW num cont (HMApp l1 l2) =
        case doAlgorithmW num cont l1 of
            Nothing -> Nothing
            Just (subst1, type1, num1) ->
                let
                  cont1 = applyMultiTypesSubstitution subst1 cont
                in
                  case doAlgorithmW num1 cont1 l2 of
                      Nothing -> Nothing
                      Just (subst2, type2, num2) ->
                        let
                          beta = genHMType num2
                          left = applyTypeSubstitution subst2 type1
                          right = HMArrow type2 beta
                        in
                          case typeUnify left right of
                              Nothing -> Nothing
                              Just v ->
                                let
                                  newSubst = compose v (compose subst1 subst2)
                                  newType = applyTypeSubstitution newSubst beta
                                  newNum = num2 + 1
                                in
                                  Just (newSubst, newType, newNum)
    doAlgorithmW num cont (HMAbs s l) =
      let
        beta = genHMType num
        cont0 = M.insert s beta cont
        num0 = num + 1
      in
        case doAlgorithmW num0 cont0 l of
            Nothing -> Nothing
            Just (subst1, type1, num1) ->
              let
                newSubst = subst1
                newType = HMArrow (applyTypeSubstitution newSubst beta) type1
                newNum = num1
              in
                Just (newSubst, newType, newNum)
    doAlgorithmW num cont (HMLet s l1 l2) =
        case doAlgorithmW num cont l1 of
            Nothing -> Nothing
            Just (subst1, type1, num1) ->
              let
                changedCont = applyMultiTypesSubstitution subst1 cont
                changedSType = closure changedCont type1
                cont1 = M.insert s changedSType changedCont
              in
                case doAlgorithmW num1 cont1 l2 of
                    Nothing -> Nothing
                    Just (subst2, type2, num2) ->
                      let
                        newSubst = compose subst2 subst1
                        newType = type2
                        newNum = num2
                      in
                        Just (newSubst, newType, newNum)
