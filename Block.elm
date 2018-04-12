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
        , removeAt
        , updateAt
        , reduce
        , testExpr
        , int
        , intlit
        )

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


exprAt : Indices -> Expr -> Maybe Expr
exprAt indices =
    let
        go : Indices -> Expr -> Maybe Expr
        go idxs e =
            if idxs == indices then
                Just e
            else
                case e of
                    Var name ->
                        Nothing

                    Hole name ->
                        Nothing

                    App f args ->
                        List.head <|
                            List.filterMap identity <|
                                List.indexedMap
                                    (\idx -> go (idxs ++ [ idx ]))
                                    args

                    Lit lit ->
                        Nothing
    in
        go []


removeAt : Indices -> Expr -> Expr
removeAt indices =
    let
        go : Indices -> Expr -> Expr
        go idxs e =
            if idxs == indices then
                -- [tmp] bogus name
                Hole "bogus"
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


updateAt : Indices -> Expr -> Expr -> Expr
updateAt indices val =
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


reduce : GetDef Expr -> Expr -> Expr
reduce getDef expr =
    case expr of
        Var name ->
            Var name

        Hole name ->
            Hole name

        App f args ->
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
