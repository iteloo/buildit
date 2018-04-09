module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Debug


type alias Name =
    String


type Expr
    = Var Name
    | App Expr Expr
    | Lam Name Expr
    | Lit Lit


type Lit
    = LInt Int
    | LBool Bool


showExpr : Expr -> String
showExpr e =
    let
        brkt str =
            "(" ++ str ++ ")"
    in
        case e of
            Var n ->
                n

            App e1 e2 ->
                brkt (showExpr e1) ++ " " ++ brkt (showExpr e2)

            Lam n e ->
                "\\" ++ n ++ "->" ++ showExpr e

            Lit lit ->
                showLit lit


showLit lit =
    case lit of
        LInt i ->
            toString i

        LBool b ->
            toString b


reduce : Expr -> ( Bool, Expr )
reduce expr =
    case expr of
        Var n ->
            ( False, Var n )

        App e1 e2 ->
            case reduce e1 of
                ( True, e1 ) ->
                    ( True, App e1 e2 )

                ( False, e1 ) ->
                    case reduce e2 of
                        ( True, e2 ) ->
                            ( True, App e1 e2 )

                        ( False, e2 ) ->
                            case e1 of
                                Lam varname e ->
                                    ( True, subs varname e2 e )

                                e ->
                                    ( False, App e1 e )

        Lam n e ->
            ( False, Lam n e )

        Lit lit ->
            ( False, Lit lit )


subs : Name -> Expr -> Expr -> Expr
subs varname inside outside =
    case outside of
        Var n ->
            if n == varname then
                inside
            else
                Var n

        App e1 e2 ->
            App (subs varname inside e1) (subs varname inside e2)

        Lam n e ->
            if n == varname then
                Debug.crash "capture!"
            else
                Lam n (subs varname inside e)

        Lit lit ->
            Lit lit


x =
    "x"


y =
    "y"


bubbleView =
    div
        [ style
            [ ( "margin", "10px" )
            , ( "border", "2px solid green" )
            ]
        ]


exprView : Expr -> Html msg
exprView e =
    case e of
        Var n ->
            bubbleView [ text n ]

        App e1 e2 ->
            bubbleView [ exprView e1, exprView e2 ]

        Lam n e ->
            bubbleView [ text ("\\" ++ n ++ " -> "), exprView e ]

        Lit lit ->
            bubbleView [ text (showLit lit) ]


main =
    let
        expr =
            (App
                (Lam x
                    (App
                        (Lam y
                            (Lit (LInt 2))
                        )
                        (Var x)
                    )
                )
                (Lit (LInt 1))
            )
    in
        div [] <|
            List.map
                (div []
                    << List.singleton
                    << exprView
                )
            <|
                List.scanl
                    (\_ -> Tuple.second << reduce)
                    expr
                    (List.range 0 10)
