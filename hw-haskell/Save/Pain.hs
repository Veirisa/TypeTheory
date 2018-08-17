module Pain where

data HashLambda = HVar String
                | HAbs String Int
                | HApp Int Int
    deriving Eq

type MapHashLambda = M.Map Int HashLambda

findMapHashLambda :: Int -> MapHashLambda -> HashLambda
findMapHashLambda = M.findWithDefault (HVar "")

-- OK
toHashLambda :: Lambda -> MapHashLambda
toHashLambda l = M.fromList $ convert 0 l
  where
    convert :: Int -> Lambda -> [(Int, HashLambda)]
    convert n (Var s)   = [(n, HVar s)]
    convert n (Abs s l') = (n, HAbs s (2 * n + 1)) : convert (2 * n + 1) l'
    convert n (App l1 l2) =
      let
        n1 = 2 * n + 1
        n2 = 2 * n + 2
      in
        (n, HApp n1 n2) : (convert n1 l1 ++ convert n2 l2)

-- OK
fromHashLambda :: MapHashLambda -> Lambda
fromHashLambda m = convert m 0
  where
    convert :: MapHashLambda -> Int -> Lambda
    convert m n = case findMapHashLambda n m of
        HVar s     -> Var s
        HAbs s n'  -> Abs s (convert m n')
        HApp n1 n2 -> App (convert m n1) (convert m n2)

hashFailsFreeToSubst :: Int -> Int -> String -> MapHashLambda -> S.Set String
hashFailsFreeToSubst sn n x m = snd $ getFailsFreeToSubst (S.fromList (freeVars sl)) l x
{-
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
        -}

hashSubstitution :: Int -> Int -> Int -> Int -> String -> MapHashLambda -> MapHashLambda
hashSubstitution num sn n pn x m =
  let
    failVars = hashFailsFreeToSubst sn n x m
    newM = renameFailsFreeVars num failVars M.empty n m
  in
    freeSubstitution sn pn (HVar x) newM
  where
    renameFailsFreeVars :: Int -> S.Set String -> M.Map String String -> Int ->
                           MapHashLambda -> MapHashLambda
    renameFailsFreeVars num fails changes n m =
        case findMapHashLambda n m of
            HVar s ->
                case M.lookup s changes of
                    Just newS -> M.insert n (HVar newS) m
                    _         -> m
            HAbs s' n' ->
              let
                newChanges =
                    if S.member s' fails && not (M.member s' changes)
                    then M.insert s'(newName num s') changes
                    else changes
                newS' =  M.findWithDefault s' s' newChanges
              in
                M.insert n (HAbs newS' n') (renameFailsFreeVars num fails newChanges n m)
            HApp n1 n2 ->
              let
                newM = renameFailsFreeVars num fails changes n1 m
              in
                renameFailsFreeVars num fails changes n2 newM

    freeSubstitution :: Int -> Int -> HashLambda -> MapHashLambda -> MapHashLambda
    freeSubstitution sn pn varX m =
        case findMapHashLambda pn m of
            HApp n1 n2 ->
              let
                left = findMapHashLambda n1 m
                right = findMapHashLambda n2 m
              in
                if left == varX
                then
                    if right == varX
                    then M.insert pn (HApp sn sn) m
                    else M.insert pn (HApp sn n2) (freeSubstitution sn n2 varX m)
                else
                    if right == varX
                    then M.insert pn (HApp n1 sn) (freeSubstitution sn n1 varX m)
                    else freeSubstitution sn n2 varX (freeSubstitution sn n1 varX m)
            HAbs s' n' ->
              let
                child = findMapHashLambda n' m
              in
                if child == varX
                then M.insert pn (HAbs s' sn) m
                else freeSubstitution sn n' varX m
            _ -> m

hashReduction :: Int -> Int -> MapHashLambda -> (MapHashLambda, Bool)
hashReduction num n m =
    case findMapHashLambda n m of
        HApp n1 n2 ->
            case findMapHashLambda n1 m of
                HAbs s1' n1' ->
                  (M.insert n --(HAbs s1' n1') (hashSubstitution num n2 n1' n1 s1' m), True)
                _ ->
                    case hashReduction num n1 m of
                        (newM, True) -> (newM, True)
                        _            -> hashReduction num n2 m
        HAbs s' n' -> hashReduction num n' m
        _ -> (m, False)

-- Сведение выражения к нормальной форме с использованием нормального порядка редукции
reduceToNormalForm :: Lambda -> Lambda
reduceToNormalForm l = doReduction 0 (toHashLambda l)
  where
    doReduction :: Int -> MapHashLambda -> Lambda
    doReduction num m =
        case hashReduction num 0 m of
            (newM, True) -> doReduction (num + 1) newM
            _            -> fromHashLambda m
