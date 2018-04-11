module Block
    exposing
        ( Expr(..)
        , Id
        , Def(..)
        , DefLhs(..)
        , DefContent(..)
        , GetDef
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
    | App Id (List Expr)
    | Lit Int


int : Type
int =
    "integer"


intlit : Int -> Expr
intlit num =
    Lit num


add23 : Expr
add23 =
    App
        "add"
        [ intlit 2
        , intlit 3
        ]


testExpr : Expr
testExpr =
    App "add" [ add23, Var "x" ]


type DefLhs
    = DefLhs Type (List DefContent)


type DefContent
    = DefVar Type Name
    | DefText String


type Def
    = Def DefLhs Expr


type alias GetDef a =
    Id -> (Type -> List DefContent -> Expr -> a) -> a


reduce : GetDef Expr -> Expr -> Expr
reduce getDef expr =
    case expr of
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

        Var name ->
            Var name

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

        App id exprs ->
            -- [tofix] capture
            App id (List.map (subst var val) exprs)

        Lit lit ->
            Lit lit
