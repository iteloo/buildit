module Block
    exposing
        ( Expr(..)
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
        , stepCallByValue
        , reduceCallByValue
        , reduceCallByValueSelection
        , reduceCallByValueSelectionAt
        , testExpr
        , int
        , intlit
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


type alias Indices =
    List Int


int : Type
int =
    "integer"


intlit : Int -> Expr
intlit num =
    Lit num


add23 : Expr
add23 =
    App "add" [ intlit 2, intlit 3 ]


addHoles : Expr
addHoles =
    App "add" [ Hole "x", Hole "y" ]


testExpr : Expr
testExpr =
    App "add" [ add23, addHoles ]


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
    -> Expr
    -> s
foldr var hole app lit e =
    case e of
        Var name ->
            var name

        Hole name ->
            hole name

        App f args ->
            app f (List.map (foldr var hole app lit) args)

        Lit x ->
            lit x


indexedFoldr :
    (Indices -> Name -> s)
    -> (Indices -> Name -> s)
    -> (Indices -> Id -> List s -> s)
    -> (Indices -> Int -> s)
    -> Expr
    -> s
indexedFoldr var hole app lit =
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


reduceCallByValue getDef expr =
    let
        result =
            stepCallByValue getDef expr
    in
        if result == expr then
            result
        else
            reduceCallByValue getDef result


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
    in
        let
            ( idxs, e ) =
                indexedFoldr var hole app lit expr
        in
            Maybe.map (((,)) e) idxs


stepCallByValue : GetDef Expr -> Expr -> Expr
stepCallByValue getDef =
    let
        var name =
            ( False, Var name )

        hole name =
            ( False, Hole name )

        app f args =
            let
                didStep =
                    List.any Tuple.first args

                args_ =
                    List.map Tuple.second args
            in
                if didStep then
                    ( True, App f args_ )
                else if f == "add" then
                    -- [hack] pick out basic operations
                    case args of
                        [ ( _, Lit x ), ( _, Lit y ) ] ->
                            ( True, Lit (x + y) )

                        _ ->
                            ( False, App f args_ )
                else
                    ( True
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

        lit x =
            ( False, Lit x )
    in
        Tuple.second << foldr var hole app lit


subst : Name -> Expr -> Expr -> Expr
subst var val expr =
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
            App id (List.map (subst var val) exprs)

        Lit lit ->
            Lit lit


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
