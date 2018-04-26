module Expr exposing (..)

import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))
import Helper
import Dict


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


{-| Case constructor args rhs
-}
type Case
    = Case Name (List Name) Expr


type alias Indices =
    List Int



-- EVALS


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
foldr var hole app lit constructor caseStmt caseBranch e =
    let
        go =
            foldr var hole app lit constructor caseStmt caseBranch
    in
        case e of
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
                caseStmt (go e)
                    (Nonempty.map
                        (\(Case c args rhs) -> caseBranch c args (go rhs))
                        cases
                    )


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
indexedFoldr var hole app lit constructor caseStmt caseBranch =
    flip
        (foldr
            (flip var)
            (flip hole)
            (\f args idxs ->
                app idxs
                    f
                    (List.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx ]))
                        args
                    )
            )
            (flip lit)
            (\c args idxs ->
                constructor idxs
                    c
                    (List.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx ]))
                        args
                    )
            )
            (\e cases idxs ->
                caseStmt idxs
                    (e (idxs ++ [ 0 ]))
                    (Nonempty.indexedMap
                        (\idx -> (|>) (idxs ++ [ idx + 1 ]))
                        cases
                    )
            )
            (\c params rhs idxs -> caseBranch c params (rhs idxs))
        )
        []


exprAt : Indices -> Expr -> Maybe Expr
exprAt =
    let
        end v left =
            case left of
                [] ->
                    Just v

                _ ->
                    Nothing
    in
        flip
            (foldr
                (end << Var)
                (end << Hole)
                (\f args left ->
                    case left of
                        [] ->
                            Maybe.map (App f) <|
                                Helper.sequenceMaybes (List.map ((|>) []) args)

                        idx :: idxs ->
                            args
                                |> List.map ((|>) idxs)
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (end << Lit)
                (\c args left ->
                    case left of
                        [] ->
                            Maybe.map (Constructor c) <|
                                Helper.sequenceMaybes (List.map ((|>) []) args)

                        idx :: idxs ->
                            args
                                |> List.map ((|>) idxs)
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (\e cases left ->
                    case left of
                        [] ->
                            Maybe.map2 CaseStmt (e []) <|
                                Helper.sequenceMaybesNonempty
                                    (Nonempty.map ((|>) []) cases)

                        idx :: idxs ->
                            (e
                                ::: Nonempty.map
                                        ((<<) (Maybe.map (\(Case _ _ rhs) -> rhs)))
                                        cases
                            )
                                |> Nonempty.map ((|>) idxs)
                                |> Nonempty.toList
                                |> List.drop idx
                                |> List.head
                                |> Maybe.andThen identity
                )
                (\c args rhs left ->
                    Maybe.map (Case c args) (rhs left)
                )
            )


setAt : Indices -> Expr -> Expr -> Expr
setAt indices val =
    let
        go : Indices -> Expr -> Expr
        go idxs e =
            if idxs == indices then
                val
            else
                case e of
                    Var name ->
                        Var name

                    Hole name ->
                        Hole name

                    App f args ->
                        App f <|
                            List.indexedMap
                                (\idx -> go (idxs ++ [ idx ]))
                                args

                    Lit lit ->
                        Lit lit

                    Constructor name args ->
                        Constructor name <|
                            List.indexedMap
                                (\idx -> go (idxs ++ [ idx ]))
                                args

                    CaseStmt e cases ->
                        CaseStmt (go (idxs ++ [ 0 ]) e) <|
                            Nonempty.indexedMap
                                (\idx (Case c params rhs) ->
                                    Case c params (go (idxs ++ [ idx + 1 ]) rhs)
                                )
                                cases
    in
        go []


removeAt : Indices -> Expr -> Expr
removeAt indices =
    -- [tmp] bogus name
    setAt indices (Hole "bogus")


updateAt : Indices -> (Expr -> Expr) -> Expr -> Expr
updateAt indices upd expr =
    case exprAt indices expr of
        Just e ->
            setAt indices (upd e) expr

        Nothing ->
            expr
