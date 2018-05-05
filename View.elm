module View exposing (view)

import Msg exposing (..)
import Model exposing (..)
import Expr
    exposing
        ( ExprA(..)
        , CaseA(..)
        )
import TypeInfer exposing (Type)
import Block
    exposing
        ( Block(..)
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
                                        case
                                            TypeInfer.inferAFull
                                                (Dict.map
                                                    (always Block.typeOfBlock)
                                                    model.blockData
                                                )
                                                e
                                        of
                                            Ok e ->
                                                exprView model.blockData
                                                    model.hover
                                                    Nothing
                                                    e

                                            Err err ->
                                                text (TypeInfer.showErr err)

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
                                litView TypeInfer.int
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
                        case
                            draftExpr
                                |> TypeInfer.inferAFull
                                    (Dict.map (always Block.typeOfBlock)
                                        model.blockData
                                    )
                        of
                            Ok expr ->
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
                                    expr

                            Err err ->
                                text (TypeInfer.showErr err)

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
                    , [ litView TypeInfer.int ]
                    ]
        ]


blkView : Expr.Name -> Block -> Html Msg
blkView id (Block blkType typ ctnts) =
    blockView
        (case blkType of
            Block.FunctionBlockType ->
                "green"

            Block.ConstructorBlockType ->
                "red"
        )
        typ
        [ onMouseDown (DragStart (LibItem id)) ]
        [ div [ style [ ( "padding", "8px" ) ] ] <|
            List.map
                (\defContent ->
                    case defContent of
                        BlkHole typ name ->
                            blockView "grey"
                                typ
                                [ style [ ( "border-style", "dashed" ) ] ]
                                [ text name
                                ]

                        BlkText txt ->
                            text txt
                )
                ctnts
        ]


litView : Type -> Html Msg
litView typ =
    blockView "orange"
        typ
        [ onMouseDown (DragStart LibLiteral)
        ]
        [ input
            [ disabled True
            , value "a number"
            ]
            []
        ]


exprView :
    BlockData
    -> Maybe Expr.Indices
    -> Maybe Expr.Indices
    -> ExprA Type
    -> Html Msg
exprView bdata hoverIdxs dragIdxs =
    let
        go : Expr.Indices -> ExprA Type -> Html Msg
        go idxs e =
            let
                -- [note] used in everything but holes
                dragStart =
                    onMouseDown (DragStart (DraftItem idxs))

                -- [note] used only in holes and tmp holes (when dragging)
                hover =
                    [ onMouseEnter (MouseOver idxs)
                    , onMouseLeave (MouseLeave idxs)
                    ]

                -- [note] used in all
                hoverHighlight =
                    case Maybe.map ((==) idxs) hoverIdxs of
                        Just True ->
                            [ style
                                [ ( "box-shadow", "0px 0px 8px grey" )
                                ]
                            ]

                        _ ->
                            []

                -- [note] used in app and caseStmt
                evalButton =
                    button [ onClick (Reduce idxs) ]
                        [ text "Evaluate" ]

                handleApp typ f args =
                    case getBlock bdata f of
                        Block blkType _ cntnts ->
                            let
                                color =
                                    case blkType of
                                        Block.FunctionBlockType ->
                                            "green"

                                        Block.ConstructorBlockType ->
                                            "red"
                            in
                                blockView color
                                    typ
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                <|
                                    [ case blkType of
                                        Block.FunctionBlockType ->
                                            evalButton

                                        Block.ConstructorBlockType ->
                                            text ""
                                    , div
                                        [ style [ ( "padding", "8px" ) ]
                                        ]
                                      <|
                                        List.reverse <|
                                            Tuple.first <|
                                                List.foldl
                                                    (\blkContent ( content, ( args, idx ) ) ->
                                                        (case blkContent of
                                                            BlkHole _ _ ->
                                                                case args of
                                                                    arg :: ags ->
                                                                        ( holeView color
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
                                                                ( text txt :: content
                                                                , ( args, idx )
                                                                )
                                                        )
                                                    )
                                                    ( [], ( args, 0 ) )
                                                    cntnts
                                    ]
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
                            VarA typ name ->
                                -- [tofix] no type information passed
                                blockView "grey"
                                    typ
                                    (List.concat
                                        [ [ dragStart ], hoverHighlight ]
                                    )
                                    [ text name ]

                            HoleA typ ->
                                blockView "grey"
                                    typ
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
                                    []

                            AppA typ f args ->
                                handleApp typ f args

                            LitA typ lit ->
                                blockView "orange"
                                    typ
                                    (List.concat
                                        [ [ dragStart
                                          ]
                                        , hoverHighlight
                                        ]
                                    )
                                    [ input
                                        [ value (toString lit)
                                        , onInput (LitEdit idxs)
                                        ]
                                        []
                                    ]

                            ConstructorA typ c args ->
                                handleApp typ c args

                            CaseStmtA typ e cases ->
                                blockView "blue"
                                    typ
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
                                                                (\idx (CaseA c params rhs) ->
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


blockView :
    String
    -> Type
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
blockView color typ attrs children =
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
        [ typeView color typ, div [] children ]


typeView : String -> Type -> Html msg
typeView color typ =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "vertical-align", "text-top" )
            , ( "background-color", color )
            , ( "font-size", "small" )
            ]
        ]
        [ text (TypeInfer.showType typ) ]


playbackView : BlockData -> Zipper EvalFrame -> Html Msg
playbackView bdata frames =
    div [] <|
        [ -- [hack] stealing dynamic view for now
          -- [todo] remove bdata once this hack is removed
          case
            Zipper.current frames
                |> Tuple.first
                |> TypeInfer.inferAFull
                    (Dict.map (always Block.typeOfBlock) bdata)
          of
            Ok expr ->
                exprView bdata
                    Nothing
                    Nothing
                    expr

            Err err ->
                text (TypeInfer.showErr err)
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
