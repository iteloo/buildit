module Block exposing (..)

import Expr exposing (..)
import TypeInfer exposing (..)
import Helper
import Debug
import List.Nonempty as Nonempty exposing (Nonempty(..), (:::))


type Block
    = Block Type (List BlkContent)


type BlkContent
    = BlkHole Type Name
    | BlkText String


type Def
    = Def (List Name) Expr


type alias GetDef a =
    Name -> (List Name -> Expr -> a) -> a


typeOfBlock : Block -> Type
typeOfBlock (Block typ cntnts) =
    rollToFuncTypes typ
        (List.filterMap
            (\cntnt ->
                case cntnt of
                    BlkHole typ _ ->
                        Just typ

                    BlkText _ ->
                        Nothing
            )
            cntnts
        )


toExpr : Name -> Block -> Expr
toExpr f (Block _ ctnts) =
    App f <|
        List.filterMap
            (\c ->
                case c of
                    BlkHole _ name ->
                        Just (Hole name)

                    BlkText _ ->
                        Nothing
            )
            ctnts


stepCBN : GetDef Expr -> Expr -> Expr
stepCBN getDef expr =
    case expr of
        Var name ->
            Var name

        Hole name ->
            Hole name

        App f args ->
            -- [hack] pick out basic operations
            if f == "add" then
                case args of
                    [ Lit x, Lit y ] ->
                        Lit (x + y)

                    _ ->
                        App f args
            else
                getDef f
                    (\params rhs ->
                        List.foldr (uncurry subst) rhs <|
                            List.map2 ((,)) params args
                    )

        Lit x ->
            Lit x

        Constructor name args ->
            Constructor name args

        CaseStmt e cases ->
            CaseStmt e cases


evalCBV : GetDef Expr -> Expr -> List ( Expr, Indices )
evalCBV getDef expr =
    Helper.generate
        (Tuple.first >> stepCBV getDef)
        ( expr, [] )
        |> Nonempty.toList


evalCBVAt :
    Indices
    -> GetDef Expr
    -> Expr
    -> List ( Expr, Indices )
evalCBVAt indices getDef expr =
    case exprAt indices expr of
        Just e ->
            evalCBV getDef e
                |> List.map
                    (\( subExpr, subIdxs ) ->
                        ( setAt indices subExpr expr
                        , indices ++ subIdxs
                        )
                    )

        Nothing ->
            Debug.crash "Indices out of bound"


type MatchErr
    = NotConstructorErr


stepCBV : GetDef Expr -> Expr -> Maybe ( Expr, Indices )
stepCBV getDef expr =
    let
        var _ name =
            ( Nothing, Var name )

        hole _ name =
            ( Nothing, Hole name )

        app idxs f args =
            let
                didStep =
                    args
                        |> List.filterMap Tuple.first
                        |> List.head

                args_ =
                    List.map Tuple.second args
            in
                case didStep of
                    Just subIdxs ->
                        ( Just subIdxs, App f args_ )

                    Nothing ->
                        if f == "add" then
                            -- [hack] pick out basic operations
                            case args of
                                [ ( _, Lit x ), ( _, Lit y ) ] ->
                                    ( Just idxs, Lit (x + y) )

                                _ ->
                                    ( Nothing, App f args_ )
                        else
                            ( Just idxs
                            , getDef f
                                (\params rhs ->
                                    List.foldr (uncurry subst) rhs <|
                                        List.map2 ((,)) params args_
                                )
                            )

        lit _ x =
            ( Nothing, Lit x )

        constructor idxs c args =
            let
                didStep =
                    args
                        |> List.filterMap Tuple.first
                        |> List.head

                args_ =
                    List.map Tuple.second args
            in
                ( didStep, Constructor c args_ )

        caseStmt idxs ( eDidStep, e ) cases =
            let
                didStep =
                    cases
                        |> Nonempty.toList
                        |> List.filterMap Tuple.first
                        |> List.head

                cases_ =
                    Nonempty.map Tuple.second cases

                matchNSub : Expr -> Case -> Maybe (Result MatchErr Expr)
                matchNSub e (Case c params rhs) =
                    case e of
                        Constructor cname args ->
                            if cname == c then
                                Just <|
                                    Ok
                                        (List.map2 ((,)) params args
                                            |> List.foldr (uncurry subst) rhs
                                        )
                            else
                                Nothing

                        _ ->
                            Just (Err NotConstructorErr)
            in
                case eDidStep of
                    Just eIdxs ->
                        ( Just eIdxs, CaseStmt e cases_ )

                    Nothing ->
                        case
                            cases_
                                |> Nonempty.toList
                                |> List.filterMap (matchNSub e)
                                |> List.head
                        of
                            Just (Ok e) ->
                                ( Just idxs, e )

                            Just (Err NotConstructorErr) ->
                                ( Nothing, e )

                            Nothing ->
                                Debug.crash "Unhandled case branch"

        cb c params rhs =
            Tuple.mapSecond (Case c params) rhs
    in
        let
            ( idxs, e ) =
                indexedFoldr var hole app lit constructor caseStmt cb expr
        in
            Maybe.map (((,)) e) idxs


subst : Name -> Expr -> Expr -> Expr
subst var val expr =
    let
        go =
            subst var val
    in
        case expr of
            Var name ->
                if name == var then
                    val
                else
                    Var name

            Hole name ->
                Hole name

            App id exprs ->
                -- [tofix] capture
                App id (List.map go exprs)

            Lit lit ->
                Lit lit

            Constructor name args ->
                Constructor name (List.map go args)

            CaseStmt e cases ->
                CaseStmt (go e) <|
                    Nonempty.map
                        (\(Case c params rhs) -> Case c params (go rhs))
                        cases
