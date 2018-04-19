module Block
    exposing
        ( Expr(..)
        , Case(..)
        , Indices
        , Id
        , Def(..)
        , DefLhs(..)
        , DefContent(..)
        , GetDef
        , defLhsToExpr
        , exprAt
        , setAt
        , removeAt
        , updateAt
        , stepCallByName
        , reduceCallByValueSelection
        , reduceCallByValueSelectionAt
        , consId
        , emptyListId
        , cons
        , emptyList
        , testExpr
        , int
        , intlit
        , string
        , list
        )

import Helper
import Debug


type alias Type =
    String


type alias Id =
    String


type alias Name =
    String


type Expr
    = Var Name
    | Hole Name
    | App Id (List Expr)
    | Lit Int
    | Constructor Name (List Expr)
    | CaseStmt Expr (List Case)


{-| Case constructor args rhs
-}
type Case
    = Case Name (List Name) Expr


type alias Indices =
    List Int


int : Type
int =
    "integer"


intlit : Int -> Expr
intlit num =
    Lit num


string : Type
string =
    "string"


list : Type -> Type
list typ =
    "list of " ++ typ


add23 : Expr
add23 =
    App "add" [ intlit 2, intlit 3 ]


addHoles : Expr
addHoles =
    App "add" [ Hole "x", Hole "y" ]


consId =
    "Cons"


emptyListId =
    "[]"


emptyList : Expr
emptyList =
    Constructor emptyListId []


cons : Expr -> Expr -> Expr
cons x y =
    Constructor "Cons" [ x, y ]


listExprFromListOfInt : List Int -> Expr
listExprFromListOfInt =
    List.foldr cons emptyList << List.map Lit


range02 : Expr
range02 =
    listExprFromListOfInt (List.range 0 2)


range56 =
    listExprFromListOfInt (List.range 5 6)


testExpr : Expr
testExpr =
    App "append" [ range02, Hole "a list" ]


type DefLhs
    = DefLhs Type (List DefContent)


type DefContent
    = DefVar Type Name
    | DefText String


type Def
    = Def DefLhs Expr


type alias GetDef a =
    Id -> (Type -> List DefContent -> Expr -> a) -> a


defLhsToExpr : Id -> DefLhs -> Expr
defLhsToExpr f (DefLhs typ ctnts) =
    App f <|
        List.filterMap
            (\c ->
                case c of
                    DefVar _ name ->
                        Just (Hole name)

                    DefText _ ->
                        Nothing
            )
            ctnts


foldr :
    (Name -> s)
    -> (Name -> s)
    -> (Id -> List s -> s)
    -> (Int -> s)
    -> (Name -> List s -> s)
    -> (s -> List t -> s)
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
                    (List.map
                        (\(Case c args rhs) -> caseBranch c args (go rhs))
                        cases
                    )


indexedFoldr :
    (Indices -> Name -> s)
    -> (Indices -> Name -> s)
    -> (Indices -> Id -> List s -> s)
    -> (Indices -> Int -> s)
    -> (Indices -> Name -> List s -> s)
    -> (Indices -> s -> List t -> s)
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
                    (List.indexedMap
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
                                sequenceMaybes (List.map ((|>) []) args)

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
                                sequenceMaybes (List.map ((|>) []) args)

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
                                sequenceMaybes (List.map ((|>) []) cases)

                        idx :: idxs ->
                            (e
                                :: List.map
                                    ((<<) (Maybe.map (\(Case _ _ rhs) -> rhs)))
                                    cases
                            )
                                |> List.map ((|>) idxs)
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
                            List.indexedMap
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


stepCallByName : GetDef Expr -> Expr -> Expr
stepCallByName getDef expr =
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
                    (\_ ctnts rhs ->
                        let
                            vars =
                                List.filterMap
                                    (\e ->
                                        case e of
                                            DefVar _ name ->
                                                Just name

                                            _ ->
                                                Nothing
                                    )
                                    ctnts

                            subs =
                                List.map2 ((,)) vars args
                        in
                            List.foldr (uncurry subst) rhs subs
                    )

        Lit lit ->
            Lit lit

        Constructor name args ->
            Constructor name args

        CaseStmt e cases ->
            CaseStmt e cases


reduceCallByValueSelection : GetDef Expr -> Expr -> List ( Expr, Indices )
reduceCallByValueSelection getDef expr =
    Helper.generate
        (Tuple.first >> stepCallByValueSelection getDef)
        ( expr, [] )


reduceCallByValueSelectionAt :
    Indices
    -> GetDef Expr
    -> Expr
    -> List ( Expr, Indices )
reduceCallByValueSelectionAt indices getDef expr =
    case exprAt indices expr of
        Just e ->
            reduceCallByValueSelection getDef e
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


stepCallByValueSelection : GetDef Expr -> Expr -> Maybe ( Expr, Indices )
stepCallByValueSelection getDef expr =
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
                                (\_ ctnts rhs ->
                                    let
                                        vars =
                                            List.filterMap
                                                (\e ->
                                                    case e of
                                                        DefVar _ name ->
                                                            Just name

                                                        _ ->
                                                            Nothing
                                                )
                                                ctnts

                                        subs =
                                            List.map2 ((,)) vars args_
                                    in
                                        List.foldr (uncurry subst) rhs subs
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
                        |> List.filterMap Tuple.first
                        |> List.head

                cases_ =
                    List.map Tuple.second cases

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
                            List.filterMap (matchNSub e) cases_
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
                    List.map
                        (\(Case c params rhs) -> Case c params (go rhs))
                        cases


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


sequenceMaybes : List (Maybe a) -> Maybe (List a)
sequenceMaybes =
    List.foldr (Maybe.map2 (::)) (Just [])
