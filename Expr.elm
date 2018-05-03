module Expr exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Helper


-- EXPR


type alias Name =
    String


type Expr
    = Var Name
    | Hole Name
    | App Name (List Expr)
    | Lit Int
    | Constructor Name (List Expr)
    | CaseStmt Expr (Nonempty Case)


type Case
    = Case Name (List Name) Expr



-- ORNAMENTED VERSION


type ExprA a
    = VarA a Name
    | HoleA a Name
    | AppA a Name (List (ExprA a))
    | LitA a Int
    | ConstructorA a Name (List (ExprA a))
    | CaseStmtA a (ExprA a) (Nonempty (CaseA a))


toExpr : ExprA a -> Expr
toExpr =
    foldrA
        (always Var)
        (always Hole)
        (always App)
        (always Lit)
        (always Constructor)
        (always CaseStmt)
        Case


toExprA : Expr -> ExprA ()
toExprA =
    foldr
        (VarA ())
        (HoleA ())
        (AppA ())
        (LitA ())
        (ConstructorA ())
        (CaseStmtA ())
        CaseA


{-| Case constructor args rhs
-}
type CaseA a
    = CaseA Name (List Name) (ExprA a)


type alias Indices =
    List Int



-- EVALS


foldrA :
    (a -> Name -> s)
    -> (a -> Name -> s)
    -> (a -> Name -> List s -> s)
    -> (a -> Int -> s)
    -> (a -> Name -> List s -> s)
    -> (a -> s -> Nonempty t -> s)
    -> (Name -> List Name -> s -> t)
    -> ExprA a
    -> s
foldrA var hole app lit constructor caseStmt caseBranch e =
    let
        go =
            foldrA var hole app lit constructor caseStmt caseBranch
    in
        case e of
            VarA a name ->
                var a name

            HoleA a name ->
                hole a name

            AppA a f args ->
                app a f (List.map go args)

            LitA a x ->
                lit a x

            ConstructorA a name args ->
                constructor a name (List.map go args)

            CaseStmtA a e cases ->
                caseStmt a
                    (go e)
                    (Nonempty.map
                        (\(CaseA c args rhs) -> caseBranch c args (go rhs))
                        cases
                    )


foldr :
    (Name -> s)
    -> (Name -> s)
    -> (Name -> List s -> s)
    -> (Int -> s)
    -> (Name -> List s -> s)
    -> (s -> Nonempty t -> s)
    -> (Name -> List Name -> s -> t)
    -> Expr
    -> s
foldr var hole app lit constructor caseStmt caseBranch expr =
    -- [note] cannot call toExpr (will result in infinite recursion)
    let
        go =
            foldr var hole app lit constructor caseStmt caseBranch
    in
        case expr of
            Var name ->
                var name

            Hole name ->
                hole name

            App f args ->
                app f (List.map go args)

            Lit x ->
                lit x

            Constructor name args ->
                constructor name (List.map go args)

            CaseStmt e cases ->
                caseStmt
                    (go e)
                    (Nonempty.map
                        (\(Case c args rhs) -> caseBranch c args (go rhs))
                        cases
                    )


indexedFoldrA :
    (Indices -> a -> Name -> s)
    -> (Indices -> a -> Name -> s)
    -> (Indices -> a -> Name -> List s -> s)
    -> (Indices -> a -> Int -> s)
    -> (Indices -> a -> Name -> List s -> s)
    -> (Indices -> a -> s -> Nonempty t -> s)
    -> (Name -> List Name -> s -> t)
    -> ExprA a
    -> s
indexedFoldrA var hole app lit constructor caseStmt caseBranch =
    flip
        (foldrA
            (\a name idxs -> var idxs a name)
            (\a name idxs -> hole idxs a name)
            (\a f args idxs ->
                app idxs
                    a
                    f
                    (List.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx ]))
                        args
                    )
            )
            (\a x idxs -> lit idxs a x)
            (\a c args idxs ->
                constructor idxs
                    a
                    c
                    (List.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx ]))
                        args
                    )
            )
            (\a e cases idxs ->
                caseStmt idxs
                    a
                    (e (idxs ++ [ 0 ]))
                    (Nonempty.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx + 1 ]))
                        cases
                    )
            )
            (\c params rhs idxs -> caseBranch c params (rhs idxs))
        )
        []


indexedFoldr :
    (Indices -> Name -> s)
    -> (Indices -> Name -> s)
    -> (Indices -> Name -> List s -> s)
    -> (Indices -> Int -> s)
    -> (Indices -> Name -> List s -> s)
    -> (Indices -> s -> Nonempty t -> s)
    -> (Name -> List Name -> s -> t)
    -> Expr
    -> s
indexedFoldr var hole app lit constructor caseStmt caseBranch expr =
    indexedFoldrA
        (always << var)
        (always << hole)
        (always << app)
        (always << lit)
        (always << constructor)
        (always << caseStmt)
        caseBranch
        (toExprA expr)


exprAtA : Indices -> ExprA a -> Maybe (ExprA a)
exprAtA =
    let
        end v left =
            case left of
                [] ->
                    Just v

                _ ->
                    Nothing
    in
        flip
            (foldrA
                (\a n -> end <| VarA a n)
                (\a n -> end <| HoleA a n)
                (\a f args left ->
                    case left of
                        [] ->
                            Maybe.map (AppA a f) <|
                                Helper.sequenceMaybes (List.map ((|>) []) args)

                        idx :: idxs ->
                            args
                                |> List.map ((|>) idxs)
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (\a x -> end <| LitA a x)
                (\a c args left ->
                    case left of
                        [] ->
                            Maybe.map (ConstructorA a c) <|
                                Helper.sequenceMaybes (List.map ((|>) []) args)

                        idx :: idxs ->
                            args
                                |> List.map ((|>) idxs)
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (\a e cases left ->
                    case left of
                        [] ->
                            Maybe.map2 (CaseStmtA a) (e []) <|
                                Helper.sequenceMaybesNonempty
                                    (Nonempty.map ((|>) []) cases)

                        idx :: idxs ->
                            (e
                                ::: Nonempty.map
                                        ((<<) (Maybe.map (\(CaseA _ _ rhs) -> rhs)))
                                        cases
                            )
                                |> Nonempty.map ((|>) idxs)
                                |> Nonempty.toList
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (\c args rhs left ->
                    Maybe.map (CaseA c args) (rhs left)
                )
            )


exprAt : Indices -> Expr -> Maybe Expr
exprAt idxs =
    toExprA >> exprAtA idxs >> Maybe.map toExpr


setAtA : Indices -> ExprA a -> ExprA a -> ExprA a
setAtA indices val =
    let
        go : Indices -> ExprA a -> ExprA a
        go idxs e =
            if idxs == indices then
                val
            else
                case e of
                    VarA a name ->
                        VarA a name

                    HoleA a name ->
                        HoleA a name

                    AppA a f args ->
                        AppA a f <|
                            List.indexedMap
                                (\idx -> go (idxs ++ [ idx ]))
                                args

                    LitA a x ->
                        LitA a x

                    ConstructorA a name args ->
                        ConstructorA a name <|
                            List.indexedMap
                                (\idx -> go (idxs ++ [ idx ]))
                                args

                    CaseStmtA a e cases ->
                        CaseStmtA a (go (idxs ++ [ 0 ]) e) <|
                            Nonempty.indexedMap
                                (\idx (CaseA c params rhs) ->
                                    CaseA c params (go (idxs ++ [ idx + 1 ]) rhs)
                                )
                                cases
    in
        go []


setAt : Indices -> Expr -> Expr -> Expr
setAt idxs val =
    toExprA >> setAtA idxs (toExprA val) >> toExpr


removeAt : Indices -> Expr -> Expr
removeAt indices =
    -- [tmp] bogus name
    -- [todo] remove parameter
    setAt indices (Hole "bogus")


updateAtA : Indices -> (ExprA a -> ExprA a) -> ExprA a -> ExprA a
updateAtA indices upd expr =
    case exprAtA indices expr of
        Just e ->
            setAtA indices (upd e) expr

        Nothing ->
            expr


updateAt : Indices -> (Expr -> Expr) -> Expr -> Expr
updateAt idxs upd =
    toExprA >> updateAtA idxs (toExpr >> upd >> toExprA) >> toExpr
