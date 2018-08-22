module Inference where

import           Types.HMLambda
import           Types.HMType
import           Types.Lambda
import           Types.SimpType

import           Unify

import           Data.List      ((\\))
import qualified Data.Map       as M (Map, delete, empty, findWithDefault,
                                      fromList, insert, lookup, map, member,
                                      singleton, toList, union)
import qualified Data.Set       as S (Set, delete, empty, singleton, toList,
                                      union)

--------------------------------------------------------------------------------

-- Вывод типа в просто типизированном лямбда-исчислении
inferSimpType :: Lambda -> Maybe ([(String, SimpType)], SimpType)
inferSimpType l =
  let
    (syst, st, _) = createSystem M.empty 0 l
  in
    case solveSystem $ map (\(st1, st2) -> (algTermOfSimpType st1, algTermOfSimpType st2)) syst of
        Just solut ->
            Just (map (fmap simpTypeOfAlgTerm) solut,
                  simpTypeOfAlgTerm $ applySubstitution solut (algTermOfSimpType st))
        _ -> Nothing
  where
    genSimpType :: Int -> SimpType
    genSimpType num = SElem $ 't' : show num

    createSystem :: M.Map String SimpType -> Int -> Lambda
                    -> ([(SimpType, SimpType)], SimpType, M.Map String SimpType)
    createSystem m num (Var s) =
        case M.lookup s m of
            Just stS -> ([], stS, m)
            _        ->
              let
                newStS = genSimpType num
              in
                ([], newStS, M.insert s newStS m)
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
        maybeOldStS = M.lookup s m
        newStS = genSimpType num
        mWithS = M.insert s newStS m
        (syst, st, m') = createSystem mWithS (2 * num + 1) l
        newSt = SArrow newStS st
      in
        case maybeOldStS of
            Just oldStS -> (syst, newSt, M.insert s oldStS m')
            _           -> (syst, newSt, M.delete s m')

--------------------------------------------------------------------------------

genHMType :: Int -> HMType
genHMType num = HMElem $ 't' : show num

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

applyTypeSubstitution :: M.Map String HMType -> HMType -> HMType
applyTypeSubstitution m tCur@(HMElem s) = M.findWithDefault tCur s m
applyTypeSubstitution m (HMArrow tl tr) =
    HMArrow (applyTypeSubstitution m tl) (applyTypeSubstitution m tr)
applyTypeSubstitution m (HMForAll s t) =
    if M.member s m
    then applyTypeSubstitution m t
    else HMForAll s (applyTypeSubstitution m t)

applyMultiTypesSubstitution :: M.Map String HMType -> M.Map String HMType -> M.Map String HMType
applyMultiTypesSubstitution subst = M.map (applyTypeSubstitution subst)

dependTypesSubstitution :: Int -> HMType -> (HMType, Int)
dependTypesSubstitution num t =
  let
    dependVars = hmTypeDependVars t
    (newVarTypesMap, newNum) = genVarTypeMap num dependVars
  in
    (applyTypeSubstitution newVarTypesMap t, newNum)
  where
    hmTypeDependVars :: HMType -> [String]
    hmTypeDependVars (HMForAll s t) = s : hmTypeDependVars t
    hmTypeDependVars _              = []

    genVarTypeMap :: Int -> [String] -> (M.Map String HMType, Int)
    genVarTypeMap num [] = (M.empty, num)
    genVarTypeMap num (x : xs) =
      let
        (newM, newNum) = genVarTypeMap (num + 1) xs
      in
        (M.insert x (genHMType num) newM, newNum)

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

compose :: M.Map String HMType -> M.Map String HMType -> M.Map String HMType
compose next prev = M.union (applyMultiTypesSubstitution next prev) next

typeUnify :: HMType -> HMType -> Maybe (M.Map String HMType)
typeUnify left right =
    case solveSystem [(algTermOfHMType left, algTermOfHMType right)] of
        Just solut -> Just $ M.fromList (map (fmap hmTypeOfAlgTerm) solut)
        _          -> Nothing

-- Вывод типа в системе Хиндли-Милнера
algorithmW :: HMLambda -> Maybe ([(String, HMType)], HMType)
algorithmW l =
    case doAlgorithmW 0 (hmLambdaFreeVarsMap l) l of
        Just (substMap, t, _) -> Just (M.toList substMap, t)
        _                     -> Nothing
  where
    doAlgorithmW :: Int -> M.Map String HMType -> HMLambda
                    -> Maybe (M.Map String HMType, HMType, Int)
    doAlgorithmW num cont (HMVar s) =
        case M.lookup s cont of
            Nothing    -> Nothing
            Just sType ->
              let
                newSubst = M.empty
                (newType, newNum) = dependTypesSubstitution num sType
              in
                Just (newSubst, newType, newNum)
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
