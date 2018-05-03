module View exposing (view)

import Msg exposing (..)
import Model exposing (..)
import Expr
    exposing
        ( Expr(..)
        , Case(..)
        )
import TypeInfer
import Block
    exposing
        ( Def(..)
        , Block(..)
        , BlkContent(..)
        )
import BlockExample
import Helper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Mouse
import Dict
import List.Zipper as Zipper exposing (Zipper)
import List.Nonempty as Nonempty


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
                                        |> Maybe.andThen (Expr.exprAt idxs)
                                of
                                    Just e ->
                                        exprView model.blockData
                                            model.hover
                                            Nothing
                                            e

                                    Nothing ->
                                        div [] []

                            LibItem id ->
                                case Dict.get id model.blockData of
                                    Nothing ->
                                        div [] []

                                    Just block ->
                                        -- [note] the itemId is unnecessary,
                                        -- and may cause unexpected behaviour
                                        blkView id block

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
            , ( "padding", "8px" )
            ]
        ]
    <|
        case model.draftExpr of
            Nothing ->
                []

            Just draftExpr ->
                [ case model.eval of
                    Nothing ->
                        exprView model.blockData
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

                    Just frames ->
                        playbackView model.blockData frames
                ]


libView model =
    div
        [ style
            [ ( "flex", "50%" )
            , ( "padding", "8px" )
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
                (div [ style [ ( "margin-bottom", "8px" ) ] ]
                    << List.singleton
                )
            <|
                List.concat
                    [ Dict.values <|
                        Dict.map blkView
                            model.blockData
                    , [ litView ]
                    ]
        ]


blkView id (Block typ ctnts) =
    blockView "green"
        [ onMouseDown (DragStart (LibItem id))
        ]
    <|
        [ typeView "green" typ
        , div [ style [ ( "padding", "8px" ) ] ] <|
            List.map
                (\defContent ->
                    (case defContent of
                        BlkHole typ name ->
                            blockView "grey"
                                [ style [ ( "border-style", "dashed" ) ] ]
                                [ typeView "grey" typ
                                , text name
                                ]

                        BlkText txt ->
                            blockView "white" [] [ text txt ]
                    )
                )
                ctnts
        ]


litView =
    blockView "orange"
        [ onMouseDown (DragStart LibLiteral)
        ]
        [ typeView "orange" TypeInfer.int
        , div []
            [ input
                [ disabled True
                , value "a number"
                ]
                []
            ]
        ]


exprView :
    BlockData
    -> Maybe Expr.Indices
    -> Maybe Expr.Indices
    -> Expr
    -> Html Msg
exprView bdata hoverIdxs dragIdxs =
    let
        go : Expr.Indices -> Expr -> Html Msg
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
                                [ ( "box-shadow", "0px 0px 8px grey" )
                                ]
                            ]

                        _ ->
                            []

                evalButton =
                    button [ onClick (Reduce idxs) ]
                        [ text "Evaluate" ]
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
                                case getBlock bdata f of
                                    Block typ cntnts ->
                                        blockView "green"
                                            (List.concat
                                                [ [ dragStart
                                                  ]
                                                , hoverHighlight
                                                ]
                                            )
                                        <|
                                            [ typeView "green" typ
                                            , evalButton
                                            , div
                                                [ style [ ( "padding", "8px" ) ]
                                                ]
                                              <|
                                                List.reverse <|
                                                    Tuple.first <|
                                                        List.foldl
                                                            (\defContent ( content, ( args, idx ) ) ->
                                                                (case defContent of
                                                                    BlkHole _ _ ->
                                                                        case args of
                                                                            arg :: ags ->
                                                                                ( holeView "green"
                                                                                    []
                                                                                    [ go
                                                                                        (idxs ++ [ idx ])
                                                                                        arg
                                                                                    ]
                                                                                    :: content
                                                                                , ( ags, idx + 1 )
                                                                                )

                                                                            [] ->
                                                                                Debug.crash
                                                                                    ("evaluation error."
                                                                                        ++ "Not enough arguments"
                                                                                    )

                                                                    BlkText txt ->
                                                                        ( blockView "white"
                                                                            []
                                                                            [ text txt ]
                                                                            :: content
                                                                        , ( args, idx )
                                                                        )
                                                                )
                                                            )
                                                            ( [], ( args, 0 ) )
                                                            cntnts
                                            ]

                            Lit lit ->
                                blockView "orange"
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                    [ typeView "orange" TypeInfer.int
                                    , div []
                                        [ input
                                            [ value (toString lit)
                                            , onInput (LitEdit idxs)
                                            ]
                                            []
                                        ]
                                    ]

                            Constructor c args ->
                                blockView "red"
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                <|
                                    List.concat
                                        [ [ text c
                                          , div
                                                [ style [ ( "padding", "8px" ) ]
                                                ]
                                            <|
                                                List.indexedMap
                                                    (\idx ->
                                                        holeView "green"
                                                            []
                                                            << List.singleton
                                                            << go (idxs ++ [ idx ])
                                                    )
                                                    args
                                          ]
                                        ]

                            CaseStmt e cases ->
                                blockView "blue"
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                <|
                                    List.singleton <|
                                        div
                                            [ style [ ( "padding", "8px" ) ]
                                            ]
                                        <|
                                            List.map (div [] << List.singleton) <|
                                                List.concat
                                                    [ [ evalButton
                                                      ]
                                                    , List.intersperse (text "------------") <|
                                                        List.concat
                                                            [ [ div []
                                                                    [ text "Pick a different thing depending on which possilibity"
                                                                    , go (idxs ++ [ 0 ]) e
                                                                    , text "is:"
                                                                    ]
                                                              ]
                                                            , List.indexedMap
                                                                (\idx (Case c params rhs) ->
                                                                    div []
                                                                        [ text "If it is "
                                                                        , (c :: params)
                                                                            |> List.intersperse " "
                                                                            |> List.foldr (++) ""
                                                                            |> text
                                                                        , text ", pick "
                                                                        , go (idxs ++ [ idx + 1 ])
                                                                            rhs
                                                                        ]
                                                                )
                                                              <|
                                                                Nonempty.toList cases
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
        [ text (TypeInfer.showType typ) ]


playbackView : BlockData -> Zipper EvalFrame -> Html Msg
playbackView bdata frames =
    div [] <|
        [ -- [hack] stealing dynamic view for now
          -- [todo] remove bdata once this hack is removed
          exprView bdata
            Nothing
            Nothing
            (Tuple.first (Zipper.current frames))
        , div []
            [ button
                (case Zipper.previous frames of
                    Just newFrames ->
                        [ disabled False, onClick (StepTo newFrames) ]

                    Nothing ->
                        [ disabled True ]
                )
                [ text "<" ]
            , button
                (case Zipper.next frames of
                    Just newFrames ->
                        [ disabled False, onClick (StepTo newFrames) ]

                    Nothing ->
                        [ disabled True ]
                )
                [ text ">" ]
            ]
        ]



-- HELPER


onMouseDown : (Mouse.Position -> msg) -> Attribute msg
onMouseDown msg =
    onWithOptions
        "mousedown"
        -- [note] needed to stop parent nodes from receiving event
        { defaultOptions | stopPropagation = True }
        (Decode.map msg Mouse.position)
