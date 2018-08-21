module Inference where

import           HMLambda
import           HMType
import           Lambda
import           SimpType
import           Unify

import qualified Data.Map as M (Map, empty, insert, lookup, union)

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
    genType :: Int -> SimpType
    genType num = SElem $ 't' : show num

    createSystem :: M.Map String SimpType -> Int -> Lambda
                    -> ([(SimpType, SimpType)], SimpType, M.Map String SimpType)
    createSystem m num (Var s) =
        case M.lookup s m of
            Just stv -> ([], stv, m)
            _        -> ([], genType num, M.insert s (genType num) m)
    createSystem m num (App l1 l2) =
      let
        (syst1, st1, m1) = createSystem m (2 * num + 1) l1
        (syst2, st2, m2) = createSystem m1 (2 * num + 2) l2
        newSt = genType num
        newEq = (st1, SArrow st2 newSt)
      in
        (newEq : syst1 ++ syst2, newSt, M.union m1 m2)
    createSystem m num (Abs s l) =
      let
        (syst, st, m') = createSystem m (2 * num + 1) l
      in
        case M.lookup s m' of
            Just stv -> (syst, SArrow stv st, m')
            _ -> (syst, SArrow (genType num) st, M.insert s (genType num) m')

--------------------------------------------------------------------------------

-- Вывода типа в системе Хиндли-Милнера
algorithmW :: HMLambda -> Maybe ([(String, HMType)], HMType)
algorithmW = undefined
