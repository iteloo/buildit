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
        [ editView model
        , libView model
        , case model.dragging of
            Just { itemId, confirmed, startPos, currentPos } ->
                if confirmed then
                    div
                        [ style
                            [ ( "position", "fixed" )
                            , ( "top", toString currentPos.y ++ "px" )
                            , ( "left", toString currentPos.x ++ "px" )
                            , ( "will-change", "transform" )
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

                            LibLiteral ->
                                litView
                        ]
                else
                    div [] []

            Nothing ->
                div [] []
        ]


editView model =
    div
        [ style
            [ ( "flex", "50%" )
            , ( "padding", "20px" )
            ]
        ]
    <|
        case model.draftExpr of
            Nothing ->
                []

            Just draftExpr ->
                [ exprView (mkGetDef model.defs)
                    model.hover
                    (model.dragging
                        |> Maybe.andThen
                            (\{ itemId, confirmed } ->
                                if confirmed then
                                    case itemId of
                                        DraftItem idxs ->
                                            Just idxs

                                        _ ->
                                            Nothing
                                else
                                    Nothing
                            )
                    )
                    draftExpr
                , playbackView (mkGetDef model.defs)
                    (mkGetDef model.defs)
                    draftExpr
                ]


libView model =
    div
        [ style
            [ ( "flex", "50%" )
            , ( "padding", "20px" )
            ]
        ]
        [ h3
            [ style
                [ ( "text-align", "center" )
                , ( "user-select", "none" )
                ]
            ]
            [ text "Library Blocks" ]
        , div [] <|
            List.map
                (div [ style [ ( "margin-bottom", "20px" ) ] ]
                    << List.singleton
                )
            <|
                List.concat
                    [ Dict.values <|
                        Dict.map
                            (\id (Def lhs _) -> defView id lhs)
                            model.defs
                    , [ litView ]
                    ]
        ]


defView id (DefLhs typ ctnts) =
    blockView "green"
        [ onMouseDown (DragStart (LibItem id))
        ]
    <|
        [ typeView "green" typ
        , div [ style [ ( "padding", "20px" ) ] ] <|
            List.map
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


litView =
    blockView "orange"
        [ onMouseDown (DragStart LibLiteral)
        ]
        [ typeView "orange" Block.int
        , div []
            [ input
                [ disabled True
                , value "a number"
                ]
                []
            ]
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

                hoverHighlight =
                    case Maybe.map ((==) idxs) hoverIdxs of
                        Just True ->
                            [ style
                                [ ( "box-shadow", "0px 0px 20px grey" )
                                ]
                            ]

                        _ ->
                            []
            in
                case Maybe.map ((==) idxs) dragIdxs of
                    Just True ->
                        div
                            (List.concat
                                [ [ style
                                        [ ( "width", "100%" )
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
                                blockView "grey"
                                    (List.concat
                                        [ [ dragStart ], hoverHighlight ]
                                    )
                                    [ text name ]

                            Hole name ->
                                blockView "grey"
                                    (List.concat
                                        [ [ onMouseDown (always HoleMouseDown)
                                          ]
                                        , hoverHighlight
                                        , hover
                                        , [ style
                                                [ ( "border-style", "dashed" )
                                                , ( "height", "100%" )
                                                ]
                                          ]
                                        , case Maybe.map ((==) idxs) hoverIdxs of
                                            Just True ->
                                                [ style
                                                    [ ( "background-color", "lightgrey" )
                                                    ]
                                                ]

                                            _ ->
                                                []
                                        ]
                                    )
                                    [ text name ]

                            App f args ->
                                getDef f
                                    (\typ ctnts _ ->
                                        blockView "green"
                                            (List.concat
                                                [ [ dragStart
                                                  ]
                                                , hoverHighlight
                                                ]
                                            )
                                        <|
                                            [ typeView "green" typ
                                            , button [ onClick (Reduce idxs) ]
                                                [ text "Evaluate" ]
                                            , div
                                                [ style [ ( "padding", "20px" ) ]
                                                ]
                                              <|
                                                List.reverse <|
                                                    (\( _, a, _ ) -> a) <|
                                                        List.foldr
                                                            (\defContent ( args, content, idx ) ->
                                                                (case defContent of
                                                                    DefVar _ _ ->
                                                                        case args of
                                                                            arg :: ags ->
                                                                                ( ags
                                                                                , holeView "green"
                                                                                    []
                                                                                    [ go
                                                                                        (idxs ++ [ idx ])
                                                                                        arg
                                                                                    ]
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
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                    [ typeView "orange" Block.int
                                    , div []
                                        [ input
                                            [ value (toString lit)
                                            , onInput (LitEdit idxs)
                                            ]
                                            []
                                        ]
                                    ]
    in
        go []


holeView color attrs =
    div
        ([ style
            [ ( "padding", "1px" )
            , ( "border", "2px solid " ++ color )
            , ( "border-radius", "10px" )
            , ( "box-shadow", "inset 0px 0px 20px " ++ color )
            , ( "background-color", "white" )
            , ( "overflow", "hidden" )
            , ( "user-select", "none" )
            ]
         ]
            ++ attrs
        )


blockView color attrs =
    div
        ([ style
            [ ( "border", "2px solid " ++ color )
            , ( "border-radius", "10px" )
            , ( "box-shadow", "inset 0px 0px 5px " ++ color )
            , ( "background-color", "white" )
            , ( "overflow", "hidden" )
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


playbackView getDefHtml getDefExpr expr =
    div [] <|
        -- [hack] stealing dynamic view for now
        List.map (exprView getDefHtml Nothing Nothing)
        <|
            generate
                (\e ->
                    let
                        result =
                            Block.stepCallByValue getDefExpr e
                    in
                        if result == e then
                            Nothing
                        else
                            Just result
                )
                expr



-- HELPER


generate : (a -> Maybe a) -> a -> List a
generate gen =
    let
        go : List a -> a -> List a
        go xs x =
            case gen x of
                Nothing ->
                    x :: xs

                Just next ->
                    go (x :: xs) next
    in
        go []


onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown msg =
    onWithOptions
        "mousedown"
        -- [note] needed to stop parent nodes from receiving event
        { defaultOptions | stopPropagation = True }
        (Decode.map msg Mouse.position)
