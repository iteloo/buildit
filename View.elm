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
import Html.Events exposing (..)
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
          <|
            case model.draftExpr of
                Nothing ->
                    []

                Just draftExpr ->
                    [ editView (mkGetDef model.defs)
                        model.hover
                        (model.dragging
                            |> Maybe.andThen
                                (\{ itemId } ->
                                    case itemId of
                                        DraftItem idxs ->
                                            Just idxs

                                        LibItem _ ->
                                            Nothing
                                )
                        )
                        draftExpr
                    ]
        , div
            [ style
                [ ( "flex", "50%" )
                ]
            ]
            [ libView model ]
        , case model.dragging of
            Just { itemId, startPos, currentPos } ->
                div
                    [ style
                        [ ( "position", "fixed" )
                        , ( "top", toString currentPos.y ++ "px" )
                        , ( "left", toString currentPos.x ++ "px" )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        , -- [hack] needed otherwise hover events of
                          -- areas to be dropped into are caught
                          ( "pointer-events", "none" )
                        ]
                    ]
                    [ case itemId of
                        DraftItem idxs ->
                            case
                                model.draftExpr
                                    |> Maybe.andThen (Block.exprAt idxs)
                            of
                                Just e ->
                                    exprView (mkGetDef model.defs)
                                        model.hover
                                        Nothing
                                        e

                                Nothing ->
                                    div [] []

                        LibItem id ->
                            case Dict.get id model.defs of
                                Nothing ->
                                    div [] []

                                Just (Def lhs _) ->
                                    -- [note] the itemId is unnecessary,
                                    -- and may cause unexpected behaviour
                                    defView id lhs
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
        [ onMouseDown (DragStart (LibItem id))
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


exprView :
    Block.GetDef (Html Msg)
    -> Maybe Block.Indices
    -> Maybe Block.Indices
    -> Expr
    -> Html Msg
exprView getDef hoverIdxs dragIdxs =
    let
        go : Block.Indices -> Expr -> Html Msg
        go idxs e =
            let
                dragStart =
                    onMouseDown (DragStart (DraftItem idxs))

                hover =
                    [ onMouseEnter (MouseOver idxs)
                    , onMouseLeave (MouseLeave idxs)
                    ]
            in
                case Maybe.map ((==) idxs) dragIdxs of
                    Just True ->
                        blockView "black"
                            (List.concat
                                [ [ style
                                        [ ( "width", "50px" )
                                        , ( "height", "50px" )
                                        ]
                                  ]
                                , hover
                                ]
                            )
                            []

                    _ ->
                        case e of
                            Var name ->
                                -- [tofix] no type information passed
                                blockView "grey" [ dragStart ] [ text name ]

                            Hole name ->
                                blockView "purple"
                                    (List.concat
                                        [ List.concat
                                            [ [ dragStart
                                              ]
                                            , hover
                                            ]
                                        , case hoverIdxs of
                                            Nothing ->
                                                []

                                            Just is ->
                                                if is == idxs then
                                                    [ style
                                                        [ ( "background-color", "black" ) ]
                                                    ]
                                                else
                                                    []
                                        ]
                                    )
                                    [ text name ]

                            App f args ->
                                getDef f
                                    (\typ ctnts _ ->
                                        blockView "green" [ dragStart ] <|
                                            List.concat
                                                [ [ typeView "green" typ ]
                                                , List.reverse <|
                                                    (\( _, a, _ ) -> a) <|
                                                        List.foldr
                                                            (\defContent ( args, content, idx ) ->
                                                                (case defContent of
                                                                    DefVar _ _ ->
                                                                        case args of
                                                                            arg :: ags ->
                                                                                ( ags
                                                                                , go
                                                                                    (idxs ++ [ idx ])
                                                                                    arg
                                                                                    :: content
                                                                                , idx + 1
                                                                                )

                                                                            [] ->
                                                                                Debug.crash
                                                                                    ("evaluation error."
                                                                                        ++ "Not enough arguments"
                                                                                    )

                                                                    DefText txt ->
                                                                        ( args
                                                                        , blockView "white"
                                                                            []
                                                                            [ text txt ]
                                                                            :: content
                                                                        , idx
                                                                        )
                                                                )
                                                            )
                                                            ( args, [], 0 )
                                                            ctnts
                                                ]
                                    )

                            Lit lit ->
                                blockView "orange"
                                    [ dragStart ]
                                    [ typeView "orange" Block.int
                                    , blockView "white" [] [ text (toString lit) ]
                                    ]
    in
        go []


blockView color attrs =
    div
        ([ style
            [ ( "margin", "10px" )
            , ( "border", "2px solid " ++ color )
            , ( "user-select", "none" )
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
    onWithOptions
        "mousedown"
        -- [note] needed to stop parent nodes from receiving event
        { defaultOptions | stopPropagation = True }
        (Decode.map msg Mouse.position)
