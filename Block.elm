module Block exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Debug


main =
    view


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


addId =
    "add"


add =
    Def
        (DefLhs int
            [ DefVar int "x"
            , DefText "+"
            , DefVar int "y"
            ]
        )
        (Var "x")


defs =
    Dict.fromList
        [ ( addId, add )
        ]


getDef id handler =
    case Dict.get id defs of
        Just (Def (DefLhs typ ctnts) rhs) ->
            handler typ ctnts rhs

        Nothing ->
            Debug.crash ("No matching definition found for " ++ id)


reduce : Expr -> Expr
reduce expr =
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


view : Html msg
view =
    div
        [ style
            [ ( "display", "flex" )
            ]
        ]
        [ div
            [ style
                [ ( "flex", "50%" )
                ]
            ]
            [ editView testExpr ]
        , div
            [ style
                [ ( "flex", "50%" )
                ]
            ]
            [ libView ]
        ]


editView =
    exprView


libView =
    div [] <|
        List.map (\(Def lhs rhs) -> defView lhs) <|
            Dict.values defs


defView (DefLhs typ ctnts) =
    blockView "green" [] <|
        List.concat
            [ [ typeView "green" typ ]
            , List.map
                (\defContent ->
                    (case defContent of
                        DefVar typ name ->
                            blockView "grey"
                                [ style [ ( "border-style", "dashed" ) ] ]
                                [ typeView "grey" typ
                                , text name
                                ]

                        DefText txt ->
                            blockView "white" [] [ text txt ]
                    )
                )
                ctnts
            ]


exprView : Expr -> Html msg
exprView e =
    case e of
        Var name ->
            -- [tofix] no type information passed
            blockView "grey" [] [ text name ]

        App f args ->
            getDef f
                (\typ ctnts _ ->
                    blockView "green" [] <|
                        List.concat
                            [ [ typeView "green" typ ]
                            , List.reverse <|
                                Tuple.second <|
                                    List.foldr
                                        (\defContent ( args, content ) ->
                                            (case defContent of
                                                DefVar _ _ ->
                                                    case args of
                                                        arg :: ags ->
                                                            ( ags
                                                            , exprView arg
                                                                :: content
                                                            )

                                                        [] ->
                                                            Debug.crash
                                                                ("evaluation error."
                                                                    ++ "Not enough arguments"
                                                                )

                                                DefText txt ->
                                                    ( args
                                                    , blockView "white" [] [ text txt ]
                                                        :: content
                                                    )
                                            )
                                        )
                                        ( args, [] )
                                        ctnts
                            ]
                )

        Lit lit ->
            blockView "orange"
                []
                [ typeView "orange" int
                , blockView "white" [] [ text (toString lit) ]
                ]


blockView color attrs =
    div
        ([ style
            [ ( "margin", "10px" )
            , ( "border", "2px solid " ++ color )
            ]
         ]
            ++ attrs
        )


typeView color typ =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "vertical-align", "text-top" )
            , ( "background-color", color )
            , ( "font-size", "x-small" )
            ]
        ]
        [ text typ ]
