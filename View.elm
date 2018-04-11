module View exposing (view)

import Model exposing (..)
import Block
    exposing
        ( Def(..)
        , DefLhs(..)
        , DefContent(..)
        , Expr(..)
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Dict


view model =
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
            [ editView (mkGetDef model.defs) Block.testExpr ]
        , div
            [ style
                [ ( "flex", "50%" )
                ]
            ]
            [ libView model ]
        ]


editView =
    exprView


libView model =
    div [] <|
        List.map (\(Def lhs rhs) -> defView lhs) <|
            Dict.values model.defs


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


exprView : Block.GetDef (Html msg) -> Expr -> Html msg
exprView getDef e =
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
                                                            , exprView getDef arg
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
                [ typeView "orange" Block.int
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
