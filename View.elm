module View exposing (view)

import Msg exposing (..)
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
import Html.Events
    exposing
        ( onClick
        , on
        , onMouseOver
        , onMouseLeave
        )
import Json.Decode as Decode
import Mouse
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
        , case model.dragging of
            Just { itemId, startPos, currentPos } ->
                case Dict.get itemId model.defs of
                    Nothing ->
                        div [] []

                    Just (Def lhs _) ->
                        div
                            [ style
                                [ ( "position", "fixed" )
                                , ( "top", toString currentPos.y ++ "px" )
                                , ( "left", toString currentPos.x ++ "px" )
                                , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                                , ( "willChange", "transform" )
                                ]
                            ]
                            [ -- [note] the itemId is unnecessary,
                              -- and may cause unexpected behaviour
                              defView itemId lhs
                            ]

            Nothing ->
                div [] []
        ]


editView =
    exprView


libView model =
    div [] <|
        Dict.values <|
            Dict.map
                (\id (Def lhs _) -> defView id lhs)
                model.defs


defView id (DefLhs typ ctnts) =
    blockView "green"
        [ onMouseDown (DragStart id)
        ]
    <|
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


exprView : Block.GetDef (Html Msg) -> Expr -> Html Msg
exprView getDef e =
    case e of
        Var name ->
            -- [tofix] no type information passed
            blockView "grey" [] [ text name ]

        Hole name ->
            blockView "purple"
                [ onMouseOver (MouseOver ("over " ++ toString name))
                , onMouseLeave (MouseLeave ("leaving " ++ toString name))
                ]
                [ text name ]

        App f args ->
            getDef f
                (\typ ctnts _ ->
                    blockView "green"
                        []
                    <|
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



-- HELPER


onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown msg =
    on "mousedown" (Decode.map msg Mouse.position)
